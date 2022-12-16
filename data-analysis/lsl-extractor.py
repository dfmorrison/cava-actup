import json

inputname = input("input filename: ")
locChanges = []
with open(inputname) as f:
    ##locChanges.append({"NextTask": "Start"})
    nextTask = "Start"
    skipTask = False
    nextID = "none"
    nextTimestamp = "none"
    ghidraEventFound = False
    for line in f:
        ##print(line)
        tmpline = json.loads(line)
        if "TaskStartEvent" in tmpline:
            ##locChanges.append({"NextTask": tmpline["TaskStartEvent"]["TaskName"]})
            nextTask = tmpline["TaskStartEvent"]["TaskName"]
            nextTask = nextTask.replace('Proof_of_Vulnerability','Point_of_Interest')
            if skipTask and ghidraEventFound:
                locChanges.append({
                    "Task": nextTask,
                    "ID": nextID,
                    "timestamp": nextTimestamp})
            skipTask = False
            ghidraEventFound = False
            nextID = "none"
            nextTimestamp = "none"
        if "TaskNextEvent" in tmpline:
            skipTask = True
        if "GhidraLocationChangedEvent" in tmpline:
            if skipTask:
                nextID = tmpline["GhidraLocationChangedEvent"]["ByteAddress"]
                nextTimestamp = tmpline["GhidraLocationChangedEvent"]["Timestamp"]
                ghidraEventFound = True
            else:
                locChanges.append({
                    "Task": nextTask,
                    "ID": tmpline["GhidraLocationChangedEvent"]["ByteAddress"],
                    "timestamp": tmpline["GhidraLocationChangedEvent"]["Timestamp"]})
outname = input("output filename: ")
with open(outname, "w") as f:
    for line in locChanges:
        f.write(json.dumps(line))
        f.write("\n")

