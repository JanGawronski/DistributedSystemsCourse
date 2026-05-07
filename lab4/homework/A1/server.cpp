#include <iostream>
#include <memory>
#include <string>
#include <thread>
#include <mutex>
#include <unordered_map>
#include <chrono>

#include <grpcpp/grpcpp.h>
#include "devices.grpc.pb.h"
#include "devices.pb.h"
#include <google/protobuf/empty.pb.h>

using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;
using grpc::ServerWriter;
using grpc::Status;
using grpc::StatusCode;

using smart::home::DeviceManager;
using smart::home::DeviceId;
using smart::home::DeviceInfo;
using smart::home::DeviceState;
using smart::home::ListDevicesResponse;
using smart::home::CommandRequest;
using smart::home::CommandResponse;
using smart::home::TemperatureRecord;

static int64_t now_ms() {
  using namespace std::chrono;
  return duration_cast<milliseconds>(system_clock::now().time_since_epoch()).count();
}

class DeviceManagerServiceImpl final : public DeviceManager::Service {
public:
  DeviceManagerServiceImpl() { init_devices(); }

  Status ListDevices(ServerContext* /*context*/, const google::protobuf::Empty* /*req*/, ListDevicesResponse* resp) override {
    std::lock_guard<std::mutex> lk(mu_);
    std::cerr << "[ListDevices] called, count=" << infos_.size() << std::endl;
    for (const auto& kv : infos_) {
      DeviceInfo* di = resp->add_devices();
      *di = kv.second;
    }
    return Status::OK;
  }

  Status GetDeviceInfo(ServerContext* /*context*/, const DeviceId* req, DeviceInfo* resp) override {
    std::lock_guard<std::mutex> lk(mu_);
    auto it = infos_.find(req->id());
    if (it == infos_.end()) {
      std::cerr << "[GetDeviceInfo] id=" << req->id() << " NOT FOUND" << std::endl;
      return Status(StatusCode::NOT_FOUND, "device not found");
    }
    std::cerr << "[GetDeviceInfo] id=" << req->id() << std::endl;
    *resp = it->second;
    return Status::OK;
  }

  Status GetDeviceState(ServerContext* /*context*/, const DeviceId* req, DeviceState* resp) override {
    std::lock_guard<std::mutex> lk(mu_);
    auto it = states_.find(req->id());
    if (it == states_.end()) {
      std::cerr << "[GetDeviceState] id=" << req->id() << " NOT FOUND" << std::endl;
      return Status(StatusCode::NOT_FOUND, "device not found");
    }
    std::cerr << "[GetDeviceState] id=" << req->id() << std::endl;
    *resp = it->second;
    return Status::OK;
  }

  Status SendCommand(ServerContext* /*context*/, const CommandRequest* req, CommandResponse* resp) override {
    std::lock_guard<std::mutex> lk(mu_);
    auto state_it = states_.find(req->id());
    auto info_it = infos_.find(req->id());
    if (state_it == states_.end() || info_it == infos_.end()) {
      resp->set_ok(false);
      resp->set_message("device not found");
      return Status(StatusCode::NOT_FOUND, "device not found");
    }
    DeviceState& st = state_it->second;
    const DeviceInfo& info = info_it->second;

    if ((req->has_fridge() && info.device_type() != "FRIDGE") ||
        (req->has_heater() && info.device_type() != "HEATER") ||
        (req->has_camera() && info.device_type() != "CAMERA")) {
      resp->set_ok(false);
      resp->set_message("command type does not match device type");
      return Status(StatusCode::INVALID_ARGUMENT, "command type does not match device type");
    }

    if (req->has_fridge()) {
      const auto& fc = req->fridge();
      if (fc.has_set_temp()) {
        double t = fc.set_temp().temperature();
        auto* fs = st.mutable_fridge();
        fs->set_temperature(t);
        TemperatureRecord* rec = fs->add_history();
        rec->set_ts(now_ms());
        rec->set_temperature(t);
        if (fs->history_size() > 20) {
          fs->mutable_history()->DeleteSubrange(0, fs->history_size() - 20);
        }
      } else if (fc.has_door()) {
        bool open = fc.door().open();
        auto* fs = st.mutable_fridge();
        fs->set_door_open(open);
      } else {
        resp->set_ok(false);
        resp->set_message("fridge command action is missing");
        return Status(StatusCode::INVALID_ARGUMENT, "fridge command action is missing");
      }
    } else if (req->has_heater()) {
      const auto& hc = req->heater();
      if (hc.has_setpoint()) {
        double sp = hc.setpoint().setpoint();
        auto* hs = st.mutable_heater();
        hs->set_setpoint(sp);
      } else if (hc.has_onoff()) {
        bool on = hc.onoff().on();
        auto* hs = st.mutable_heater();
        hs->set_is_on(on);
      } else {
        resp->set_ok(false);
        resp->set_message("heater command action is missing");
        return Status(StatusCode::INVALID_ARGUMENT, "heater command action is missing");
      }
    } else if (req->has_camera()) {
      const auto& cc = req->camera();
      if (cc.has_ptz()) {
        auto* cs = st.mutable_camera();
        cs->set_pan(cc.ptz().pan());
        cs->set_tilt(cc.ptz().tilt());
        cs->set_zoom(cc.ptz().zoom());
      } else if (cc.has_record()) {
        auto* cs = st.mutable_camera();
        cs->set_recording(cc.record().start());
      } else {
        resp->set_ok(false);
        resp->set_message("camera command action is missing");
        return Status(StatusCode::INVALID_ARGUMENT, "camera command action is missing");
      }
    } else {
      resp->set_ok(false);
      resp->set_message("unknown command");
      return Status(StatusCode::INVALID_ARGUMENT, "unknown command");
    }

    st.set_timestamp(now_ms());
    resp->set_ok(true);
    resp->set_message("ok");
    *resp->mutable_state() = st;
    std::cerr << "[SendCommand] id=" << req->id() << " updated\n";
    return Status::OK;
  }

  Status StreamDeviceState(ServerContext* context, const DeviceId* req, ServerWriter<DeviceState>* writer) override {
    std::string id = req->id();
    std::cerr << "[StreamDeviceState] subscribe id=" << id << std::endl;
    while (!context->IsCancelled()) {
      DeviceState copy;
      {
        std::lock_guard<std::mutex> lk(mu_);
        auto it = states_.find(id);
        if (it == states_.end()) {
          std::cerr << "[StreamDeviceState] id=" << id << " not found" << std::endl;
          return Status(StatusCode::NOT_FOUND, "device not found");
        }
        copy = it->second;
      }
      std::cerr << "[StreamDeviceState] id=" << id << " sending ts=" << copy.timestamp() << std::endl;
      if (!writer->Write(copy)) {
        return Status::OK;
      }
      std::this_thread::sleep_for(std::chrono::seconds(1));
    }
    std::cerr << "[StreamDeviceState] id=" << id << " cancelled" << std::endl;
    return Status::OK;
  }

private:
  void init_devices() {
    // 4 fridges
    for (int i = 1; i <= 4; ++i) {
      std::string id = "fridge-" + std::to_string(i);
      DeviceInfo di;
      di.set_id(id);
      di.set_device_type("FRIDGE");
      di.set_subtype((i % 2 == 0) ? "large" : "small");
      di.set_location("room-" + std::to_string((i % 2) + 1));
      (*di.mutable_metadata())["vendor"] = "Acme";
      (*di.mutable_metadata())["model"] = "F" + std::to_string(i);

      DeviceState st;
      st.set_id(id);
      auto* fs = st.mutable_fridge();
      fs->set_temperature(4.0 + i);
      fs->set_door_open(false);
      TemperatureRecord* rec = fs->add_history();
      rec->set_ts(now_ms());
      rec->set_temperature(fs->temperature());
      st.set_timestamp(now_ms());

      infos_.emplace(id, std::move(di));
      states_.emplace(id, std::move(st));
    }

    // 3 heaters
    for (int i = 1; i <= 3; ++i) {
      std::string id = "heater-" + std::to_string(i);
      DeviceInfo di;
      di.set_id(id);
      di.set_device_type("HEATER");
      di.set_subtype((i % 2 == 0) ? "electric" : "gas");
      di.set_location("floor-" + std::to_string(i));
      (*di.mutable_metadata())["vendor"] = "HeatCo";
      (*di.mutable_metadata())["model"] = "H" + std::to_string(i);

      DeviceState st;
      st.set_id(id);
      auto* hs = st.mutable_heater();
      hs->set_setpoint(21.0 + i);
      hs->set_current_temp(18.0 + i);
      hs->set_is_on(false);
      st.set_timestamp(now_ms());

      infos_.emplace(id, std::move(di));
      states_.emplace(id, std::move(st));
    }

    // 3 cameras
    for (int i = 1; i <= 3; ++i) {
      std::string id = "camera-" + std::to_string(i);
      DeviceInfo di;
      di.set_id(id);
      di.set_device_type("CAMERA");
      di.set_subtype((i % 2 == 0) ? "outdoor" : "indoor");
      di.set_location("area-" + std::to_string(i));
      (*di.mutable_metadata())["vendor"] = "CamCorp";
      (*di.mutable_metadata())["model"] = "C" + std::to_string(i);

      DeviceState st;
      st.set_id(id);
      auto* cs = st.mutable_camera();
      cs->set_pan(0.0);
      cs->set_tilt(0.0);
      cs->set_zoom(1.0);
      cs->set_recording(false);
      st.set_timestamp(now_ms());

      infos_.emplace(id, std::move(di));
      states_.emplace(id, std::move(st));
    }
  }

  std::mutex mu_;
  std::unordered_map<std::string, DeviceInfo> infos_;
  std::unordered_map<std::string, DeviceState> states_;
};

int main(int argc, char** argv) {
  std::string port = (argc > 1) ? argv[1] : "50051";
  std::string server_address = "0.0.0.0:" + port;

  DeviceManagerServiceImpl service;

  ServerBuilder builder;
  builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
  builder.RegisterService(&service);
  std::unique_ptr<Server> server(builder.BuildAndStart());
  std::cerr << "Server listening on " << server_address << std::endl;
  server->Wait();

  return 0;
}
