from __future__ import annotations

import argparse
import math
import os
import shlex
from typing import Dict, List, Tuple

import ray
from ray.exceptions import RayActorError


def _chunk_data(data: str, block_size: int) -> List[str]:
    if block_size <= 0:
        raise ValueError("block_size must be > 0")
    return [data[i : i + block_size] for i in range(0, len(data), block_size)]


def _print_title(title: str) -> None:
    print(f"\n=== {title} ===")


@ray.remote
class DataNode:
    def __init__(self, node_id: int):
        self.node_id = node_id
        self.status = "UP"
        self.blocks: Dict[str, Dict[int, str]] = {}

    def _ensure_up(self) -> None:
        if self.status != "UP":
            raise RuntimeError(f"DataNode {self.node_id} is {self.status}")

    def add_block(self, name: str, block_index: int, data: str) -> None:
        self._ensure_up()
        self.blocks.setdefault(name, {})[block_index] = data

    def get_block(self, name: str, block_index: int) -> str:
        self._ensure_up()
        return self.blocks[name][block_index]

    def remove_block(self, name: str, block_index: int) -> None:
        self._ensure_up()
        if name in self.blocks and block_index in self.blocks[name]:
            del self.blocks[name][block_index]
            if not self.blocks[name]:
                del self.blocks[name]

    def set_status(self, status: str) -> str:
        self.status = status
        return self.status

    def list_blocks(self) -> Dict[str, Dict[int, int]]:
        return {
            "node_id": self.node_id,
            "status": self.status,
            "blocks": {
                name: {idx: len(value) for idx, value in sorted(blocks.items())}
                for name, blocks in self.blocks.items()
            },
        }

    def shutdown(self) -> None:
        self.status = "DOWN"
        ray.actor.exit_actor()


@ray.remote
class NameNode:
    def __init__(self, num_datanodes: int, replication_size: int, block_size: int):
        if replication_size <= 0:
            raise ValueError("replication_size must be > 0")
        self.block_size = block_size
        self.replication_size = replication_size
        self.datanodes = [DataNode.remote(node_id=i) for i in range(num_datanodes)]
        self.datanode_status = {i: "UP" for i in range(num_datanodes)}
        self.datanode_blocks: Dict[int, Dict[str, set[int]]] = {
            i: {} for i in range(num_datanodes)
        }
        self.artifacts: Dict[str, Dict[str, object]] = {}
        self._placement_cursor = 0

    def get_datanode_handles(self) -> List[ray.actor.ActorHandle]:
        return self.datanodes

    def get_block_size(self) -> int:
        return self.block_size

    def get_replication_size(self) -> int:
        return self.replication_size

    @ray.method(num_returns=2)
    def list_state(self) -> Tuple[Dict[int, str], Dict[str, Dict[str, object]]]:
        return self.datanode_status, self.artifacts

    def get_artifact_metadata(self, name: str) -> Dict[str, object]:
        if name not in self.artifacts:
            raise KeyError(f"Artifact '{name}' does not exist.")
        return self.artifacts[name]

    def plan_put(self, name: str, size: int) -> Dict[str, object]:
        if name in self.artifacts:
            raise ValueError(f"Artifact '{name}' already exists.")
        num_blocks = math.ceil(size / self.block_size) if size > 0 else 0
        plan = {
            index: self._choose_datanodes(self.replication_size, exclude=set())
            for index in range(num_blocks)
        }
        self.artifacts[name] = {
            "size": size,
            "num_blocks": num_blocks,
            "blocks": {index: [] for index in range(num_blocks)},
        }
        return {"block_size": self.block_size, "blocks": plan}

    def update_size(self, name: str, new_size: int) -> int:
        meta = self.get_artifact_metadata(name)
        new_num_blocks = math.ceil(new_size / self.block_size) if new_size > 0 else 0
        current_blocks = meta["num_blocks"]
        if new_num_blocks > current_blocks:
            for index in range(current_blocks, new_num_blocks):
                meta["blocks"][index] = []
        meta["size"] = new_size
        meta["num_blocks"] = new_num_blocks
        return new_num_blocks

    def plan_block_nodes(self, preferred_nodes: List[int]) -> List[int]:
        active = [n for n in preferred_nodes if self.datanode_status.get(n) == "UP"]
        if len(active) >= self.replication_size:
            return active[: self.replication_size]
        needed = self.replication_size - len(active)
        additional = self._choose_datanodes(needed, exclude=set(active))
        return active + additional

    def commit_block(self, name: str, block_index: int, node_ids: List[int]) -> List[int]:
        meta = self.get_artifact_metadata(name)
        if block_index >= meta["num_blocks"]:
            raise IndexError("Block index out of range.")
        old_nodes = meta["blocks"].get(block_index, [])
        for node in old_nodes:
            self._drop_block_from_node(node, name, block_index)
        meta["blocks"][block_index] = list(node_ids)
        for node in node_ids:
            self.datanode_blocks[node].setdefault(name, set()).add(block_index)
        return meta["blocks"][block_index]

    def drop_block(self, name: str, block_index: int) -> List[int]:
        meta = self.get_artifact_metadata(name)
        nodes = meta["blocks"].pop(block_index, [])
        for node in nodes:
            self._drop_block_from_node(node, name, block_index)
        return nodes

    def delete_artifact(self, name: str) -> None:
        meta = self.get_artifact_metadata(name)
        for block_index in list(meta["blocks"].keys()):
            self.drop_block(name, block_index)
        self.artifacts.pop(name, None)

    def mark_datanode_down(self, node_id: int) -> None:
        self._validate_node(node_id)
        self.datanode_status[node_id] = "DOWN"

    def mark_datanode_up(self, node_id: int) -> None:
        self._validate_node(node_id)
        self.datanode_status[node_id] = "UP"

    def plan_repair_for_failed_node(self, node_id: int) -> List[Dict[str, int]]:
        self._validate_node(node_id)
        if self.datanode_status[node_id] != "DOWN":
            raise ValueError("Node must be marked DOWN before repair.")
        repair_tasks: List[Dict[str, int]] = []
        affected = list(self.datanode_blocks[node_id].items())
        for name, blocks in affected:
            for block_index in list(blocks):
                meta = self.get_artifact_metadata(name)
                if block_index not in meta["blocks"]:
                    continue
                if node_id in meta["blocks"][block_index]:
                    meta["blocks"][block_index].remove(node_id)
                current_nodes = [
                    n
                    for n in meta["blocks"][block_index]
                    if self.datanode_status.get(n) == "UP"
                ]
                needed = self.replication_size - len(current_nodes)
                if needed <= 0:
                    continue
                targets = self._choose_datanodes(
                    needed, exclude=set(current_nodes) | {node_id}
                )
                if not current_nodes:
                    raise RuntimeError(
                        f"No available replicas for {name}:{block_index}."
                    )
                source = current_nodes[0]
                for target in targets:
                    repair_tasks.append(
                        {
                            "artifact": name,
                            "block_index": block_index,
                            "source_node": source,
                            "target_node": target,
                        }
                    )
        self.datanode_blocks[node_id] = {}
        return repair_tasks

    def commit_replica(self, name: str, block_index: int, node_id: int) -> None:
        meta = self.get_artifact_metadata(name)
        meta["blocks"].setdefault(block_index, [])
        if node_id not in meta["blocks"][block_index]:
            meta["blocks"][block_index].append(node_id)
        self.datanode_blocks[node_id].setdefault(name, set()).add(block_index)

    def _drop_block_from_node(self, node_id: int, name: str, block_index: int) -> None:
        self.datanode_blocks[node_id].setdefault(name, set()).discard(block_index)
        if not self.datanode_blocks[node_id].get(name):
            self.datanode_blocks[node_id].pop(name, None)

    def _validate_node(self, node_id: int) -> None:
        if node_id not in self.datanode_status:
            raise KeyError(f"Unknown DataNode id: {node_id}")

    def _choose_datanodes(self, count: int, exclude: set[int]) -> List[int]:
        candidates = [
            node_id
            for node_id, status in self.datanode_status.items()
            if status == "UP" and node_id not in exclude
        ]
        if len(candidates) < count:
            raise RuntimeError("Not enough healthy DataNodes for replication.")
        start = self._placement_cursor % len(candidates)
        ordered = candidates[start:] + candidates[:start]
        self._placement_cursor += count
        return ordered[:count]


def _list_state(namenode: ray.actor.ActorHandle, datanodes: List[ray.actor.ActorHandle]) -> None:
    status_ref, artifacts_ref = namenode.list_state.remote()
    datanode_status, artifacts = ray.get([status_ref, artifacts_ref])
    print("NameNode metadata:", artifacts)
    for node_id, status in datanode_status.items():
        if status == "UP":
            info = ray.get(datanodes[node_id].list_blocks.remote())
            print(f"DataNode {node_id}:", info)
        else:
            print(f"DataNode {node_id}: {{'node_id': {node_id}, 'status': '{status}'}}")


def _list_datanode(
    node_id: int,
    namenode: ray.actor.ActorHandle,
    datanodes: List[ray.actor.ActorHandle],
) -> None:
    status_ref, _ = namenode.list_state.remote()
    datanode_status = ray.get(status_ref)
    status = datanode_status.get(node_id)
    if status is None:
        raise KeyError(f"Unknown DataNode id: {node_id}")
    if status == "UP":
        info = ray.get(datanodes[node_id].list_blocks.remote())
        print(f"DataNode {node_id}:", info)
    else:
        print(f"DataNode {node_id}: {{'node_id': {node_id}, 'status': '{status}'}}")


def _list_metadata(namenode: ray.actor.ActorHandle) -> None:
    _, artifacts_ref = namenode.list_state.remote()
    artifacts = ray.get(artifacts_ref)
    print("NameNode metadata:", artifacts)


def _put_artifact(
    name: str,
    data: str,
    namenode: ray.actor.ActorHandle,
    datanodes: List[ray.actor.ActorHandle],
) -> None:
    plan = ray.get(namenode.plan_put.remote(name, len(data)))
    block_size = plan["block_size"]
    blocks = _chunk_data(data, block_size)
    for index, block in enumerate(blocks):
        node_ids = plan["blocks"][index]
        ray.get([datanodes[node_id].add_block.remote(name, index, block) for node_id in node_ids])
        ray.get(namenode.commit_block.remote(name, index, node_ids))


def _get_artifact(
    name: str,
    namenode: ray.actor.ActorHandle,
    datanodes: List[ray.actor.ActorHandle],
) -> str:
    status_ref, artifacts_ref = namenode.list_state.remote()
    datanode_status, artifacts = ray.get([status_ref, artifacts_ref])
    meta = artifacts.get(name)
    if not meta:
        raise KeyError(f"Artifact '{name}' not found.")
    blocks = []
    for index in range(meta["num_blocks"]):
        node_ids = meta["blocks"][index]
        block = None
        for node_id in node_ids:
            if datanode_status.get(node_id) != "UP":
                continue
            block = ray.get(datanodes[node_id].get_block.remote(name, index))
            break
        if block is None:
            raise RuntimeError(f"No available replica for {name}:{index}.")
        blocks.append(block)
    return "".join(blocks)[: meta["size"]]


def _update_artifact(
    name: str,
    new_data: str,
    namenode: ray.actor.ActorHandle,
    datanodes: List[ray.actor.ActorHandle],
) -> List[int]:
    block_size = ray.get(namenode.get_block_size.remote())
    old_data = _get_artifact(name, namenode, datanodes)
    old_blocks = _chunk_data(old_data, block_size)
    new_blocks = _chunk_data(new_data, block_size)
    ray.get(namenode.update_size.remote(name, len(new_data)))

    changed_indices = [
        index
        for index in range(min(len(old_blocks), len(new_blocks)))
        if old_blocks[index] != new_blocks[index]
    ]
    added_indices = list(range(len(old_blocks), len(new_blocks)))
    removed_indices = list(range(len(new_blocks), len(old_blocks)))

    status_ref, artifacts_ref = namenode.list_state.remote()
    datanode_status, artifacts = ray.get([status_ref, artifacts_ref])
    meta = artifacts[name]

    for index in changed_indices + added_indices:
        preferred = meta["blocks"].get(index, [])
        node_ids = ray.get(namenode.plan_block_nodes.remote(preferred))
        ray.get([datanodes[node_id].add_block.remote(name, index, new_blocks[index]) for node_id in node_ids])
        ray.get(namenode.commit_block.remote(name, index, node_ids))

    for index in removed_indices:
        for node_id in meta["blocks"].get(index, []):
            if datanode_status.get(node_id) == "UP":
                ray.get(datanodes[node_id].remove_block.remote(name, index))
        ray.get(namenode.drop_block.remote(name, index))

    return changed_indices + added_indices


def _delete_artifact(
    name: str,
    namenode: ray.actor.ActorHandle,
    datanodes: List[ray.actor.ActorHandle],
) -> None:
    status_ref, artifacts_ref = namenode.list_state.remote()
    datanode_status, artifacts = ray.get([status_ref, artifacts_ref])
    meta = artifacts.get(name)
    if not meta:
        raise KeyError(f"Artifact '{name}' not found.")
    for block_index, node_ids in meta["blocks"].items():
        refs = [
            datanodes[node_id].remove_block.remote(name, block_index)
            for node_id in node_ids
            if datanode_status.get(node_id) == "UP"
        ]
        if refs:
            ray.get(refs)
    ray.get(namenode.delete_artifact.remote(name))


def _repair_after_failure(
    namenode: ray.actor.ActorHandle,
    datanodes: List[ray.actor.ActorHandle],
    failed_node: int,
) -> None:
    tasks = ray.get(namenode.plan_repair_for_failed_node.remote(failed_node))
    for task in tasks:
        data = ray.get(
            datanodes[task["source_node"]].get_block.remote(
                task["artifact"], task["block_index"]
            )
        )
        ray.get(
            datanodes[task["target_node"]].add_block.remote(
                task["artifact"], task["block_index"], data
            )
        )
        ray.get(
            namenode.commit_replica.remote(
                task["artifact"], task["block_index"], task["target_node"]
            )
        )


def _delete_datanode(
    node_id: int,
    namenode: ray.actor.ActorHandle,
    datanodes: List[ray.actor.ActorHandle],
) -> None:
    ray.get(namenode.mark_datanode_down.remote(node_id))
    try:
        ray.get(datanodes[node_id].shutdown.remote())
    except RayActorError:
        print(f"DataNode {node_id} terminated.")
    _repair_after_failure(namenode, datanodes, node_id)


def _print_help() -> None:
    print(
        "\nCommands:\n"
        "  add <name> <data>\n"
        "  update <name> <data>\n"
        "  get <name>\n"
        "  list [datanode_id|namenode]\n"
        "  delete <name|datanode_id>\n"
        "  delete-datanode <datanode_id>\n"
        "  help\n"
        "  exit\n\n"
        "Note: If <data> has spaces, wrap it in quotes."
    )


def main() -> None:
    parser = argparse.ArgumentParser(description="Ray-based distributed artifact store demo.")
    parser.add_argument("--address", default=os.environ.get("RAY_ADDRESS"))
    parser.add_argument("--datanodes", type=int, default=4)
    parser.add_argument("--replication", type=int, default=2)
    parser.add_argument("--block-size", type=int, default=8)
    parser.add_argument("--no-auto-list", action="store_true")
    args = parser.parse_args()

    if ray.is_initialized():
        ray.shutdown()

    if args.address:
        ray.init(address=args.address, namespace="hdfs", ignore_reinit_error=True)
    else:
        ray.init(namespace="hdfs", ignore_reinit_error=True)

    namenode = NameNode.remote(args.datanodes, args.replication, args.block_size)
    datanodes = ray.get(namenode.get_datanode_handles.remote())

    auto_list = not args.no_auto_list
    _print_help()

    while True:
        try:
            line = input("\nhdfs> ").strip()
        except EOFError:
            print()
            break
        if not line:
            continue
        parts = shlex.split(line)
        cmd = parts[0].lower()
        args_list = parts[1:]

        if cmd in {"exit", "quit"}:
            break
        if cmd in {"help", "?"}:
            _print_help()
            continue

        try:
            if cmd == "add":
                if len(args_list) < 2:
                    raise ValueError("Usage: add <name> <data>")
                name = args_list[0]
                data = " ".join(args_list[1:])
                _put_artifact(name, data, namenode, datanodes)
                print(f"Added artifact '{name}'.")
                if auto_list:
                    _list_state(namenode, datanodes)
            elif cmd == "update":
                if len(args_list) < 2:
                    raise ValueError("Usage: update <name> <data>")
                name = args_list[0]
                data = " ".join(args_list[1:])
                changed = _update_artifact(name, data, namenode, datanodes)
                print(f"Updated artifact '{name}', blocks: {changed}.")
                if auto_list:
                    _list_state(namenode, datanodes)
            elif cmd == "get":
                if len(args_list) != 1:
                    raise ValueError("Usage: get <name>")
                name = args_list[0]
                print(_get_artifact(name, namenode, datanodes))
            elif cmd == "list":
                if not args_list:
                    _list_state(namenode, datanodes)
                else:
                    if args_list[0].lower() == "namenode":
                        _list_metadata(namenode)
                    else:
                        node_id = int(args_list[0])
                        _list_datanode(node_id, namenode, datanodes)
            elif cmd in {"delete", "del"}:
                if len(args_list) != 1:
                    raise ValueError("Usage: delete <name|datanode_id>")
                target = args_list[0]
                if target.isdigit():
                    node_id = int(target)
                    _delete_datanode(node_id, namenode, datanodes)
                    print(f"Deleted DataNode {node_id}.")
                    if auto_list:
                        _list_state(namenode, datanodes)
                else:
                    _delete_artifact(target, namenode, datanodes)
                    print(f"Deleted artifact '{target}'.")
                    if auto_list:
                        _list_state(namenode, datanodes)
            elif cmd == "delete-datanode":
                if len(args_list) != 1:
                    raise ValueError("Usage: delete-datanode <datanode_id>")
                node_id = int(args_list[0])
                _delete_datanode(node_id, namenode, datanodes)
                print(f"Deleted DataNode {node_id}.")
                if auto_list:
                    _list_state(namenode, datanodes)
            else:
                raise ValueError(f"Unknown command: {cmd}")
        except (ValueError, KeyError, RuntimeError, RayActorError) as exc:
            print(f"Error: {exc}")
            continue


if __name__ == "__main__":
    main()
