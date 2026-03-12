from fastapi import FastAPI, status
from pydantic import BaseModel
from fastapi.responses import JSONResponse

app = FastAPI()

class PollOption(BaseModel):
    description: str
    votes: set[str] = set()

class Poll(BaseModel):
    created_by: str 
    description: str
    options: dict[int, PollOption] = {}

polls = {}

@app.get("/polls/")
async def get_polls():
    return polls

@app.get("/polls/{id}")
async def get_poll(id: int):
    if id not in polls:
        return JSONResponse(status_code = status.HTTP_404_NOT_FOUND, content=f"Could not find poll with id {id}")
    return polls[id]

@app.post("/polls/")
async def create_poll(poll: Poll):
    id = max(list(polls.keys()) + [0])
    poll["id"] = id
    polls[id] = poll
    return poll

@app.post("/polls/{user}/{description}")
async def create_poll(user: str, description: str):
    id = max(list(polls.keys()) + [0])
    new_poll = {"id": id, "created_by": user, "description": description, "options": {}}
    polls[id] = new_poll
    return new_poll

@app.put("/polls/{id}/{description}")
async def edit_poll(id: int, description: str):
    if id not in polls:
        return JSONResponse(status_code = status.HTTP_404_NOT_FOUND, content=f"Could not find poll with id {id}")

    polls[id]["description"] = description
    return polls[id]

@app.delete("/polls/{id}")
async def delete_poll(id: int):
    if id in polls:
        polls.pop(id)
    return polls
    
@app.post("/polls/{id}/option/{description}")
async def create_option(id: int, description: str):
    if id not in polls:
        return JSONResponse(status_code = status.HTTP_404_NOT_FOUND, content=f"Could not find poll with id {id}")

    option_id = max(list(polls[id]["options"].keys()) + [0])
    polls[id]["options"][option_id] = {"id": option_id, "description": description, "votes": set()}

    return polls[id]

@app.patch("/polls/{id}/option/{optionid}/{description}")
async def edit_option(id: int, optionid: int, desciption: str):
    if id not in polls:
        return JSONResponse(status_code = status.HTTP_404_NOT_FOUND, content=f"Could not find poll with id {id}")
    if optionid not in polls[id]["options"]:
        return JSONResponse(status_code = status.HTTP_404_NOT_FOUND, content=f"Could not find option with id {optionid} for poll with id {id}")

    polls[id]["options"][optionid]["description"] = desciption

    return polls[id]


@app.delete("/polls/{id}/option/{optionid}")
async def delete_option(id: int, optionid: int):
    if id not in polls:
        return JSONResponse(status_code = status.HTTP_404_NOT_FOUND, content=f"Could not find poll with id {id}")

    if optionid in polls[id]["options"]:
        polls[id]["options"].pop(optionid)
        
    return polls[id]
    
@app.post("/polls/{id}/vote/{optionid}/{user}")
async def vote(id: int, optionid: int, user: str):
    if id not in polls:
        return JSONResponse(status_code = status.HTTP_404_NOT_FOUND, content=f"Could not find poll with id {id}")
    if optionid not in polls[id]["options"]:
        return JSONResponse(status_code = status.HTTP_404_NOT_FOUND, content=f"Could not find option with id {optionid} for poll with id {id}")

    polls[id]["options"][optionid]["votes"].add(user)

    return polls[id]

@app.delete("/polls/{id}/vote/{optionid}/{user}")
async def delete_vote(id: int, optionid: int, user: str):
    if id not in polls:
        return JSONResponse(status_code = status.HTTP_404_NOT_FOUND, content=f"Could not find poll with id {id}")
    if optionid not in polls[id]["options"]:
        return JSONResponse(status_code = status.HTTP_404_NOT_FOUND, content=f"Could not find option with id {optionid} for poll with id {id}")

    polls[id]["options"][optionid]["votes"].discard(user)

    return polls[id]

