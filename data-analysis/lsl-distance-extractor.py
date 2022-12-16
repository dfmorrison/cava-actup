import json

inputname = input("input filename: ")
locChanges = []
with open(inputname) as f:
    ##locChanges.append({"NextTask": "Start"})
    nextTask = "Start"
    for line in f:
        ##print(line)
        tmpline = json.loads(line)
        if "TaskStartEvent" in tmpline:
            ##locChanges.append({"NextTask": tmpline["TaskStartEvent"]["TaskName"]})
            nextTask = tmpline["TaskStartEvent"]["TaskName"]
            nextTask = nextTask.replace('Proof_of_Vulnerability','Point_of_Interest')
        if "GraphDistanceEvent" in tmpline:
            if tmpline["GraphDistanceEvent"]["TaskDescription"][:10]!="Distractor" and nextTask[:5]=="Point" and tmpline["GraphDistanceEvent"]["taskID"]>=240:
                locChanges.append({
                    "Task": nextTask,
                    "timestamp": tmpline["GraphDistanceEvent"]["Timestamp"],
                    "ClickedID": tmpline["GraphDistanceEvent"]["ClickedAddress"],
                    "KeyID": tmpline["GraphDistanceEvent"]["KeyAddress"],
                    "AssemblyDist": tmpline["GraphDistanceEvent"]["AssemblyDistance"],
                    "BlockDist": tmpline["GraphDistanceEvent"]["BlockDistance"],
                    "FunctionDist": tmpline["GraphDistanceEvent"]["FunctionDistance"]})
outname = input("output filename: ")
with open(outname, "w") as f:
    for line in locChanges:
        f.write(json.dumps(line))
        f.write("\n")

