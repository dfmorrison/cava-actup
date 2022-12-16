import json

inputname = input("input filename: ")
locChanges = []
with open(inputname) as f:
    for line in f:
        ##print(line)
        tmpline = json.loads(line)
        if "CavaPerformanceMetric" in tmpline:
            if tmpline["CavaPerformanceMetric"]["keyAddressInfo"][:10]!="Distractor":
                locChanges.append({
                    "Task": tmpline["CavaPerformanceMetric"]["TaskName"],
                    "timestamp": tmpline["CavaPerformanceMetric"]["Timestamp"],
                    "ClickedID": tmpline["CavaPerformanceMetric"]["ClickedAddress"],
                    "KeyID": tmpline["CavaPerformanceMetric"]["KeyAddress"],
                    "AssemblyDist": tmpline["CavaPerformanceMetric"]["AssemblyDistance"],
                    "BlockDist": tmpline["CavaPerformanceMetric"]["BlockDistance"],
                    "FunctionDist": tmpline["CavaPerformanceMetric"]["FunctionDistance"]})
outname = input("output filename: ")
with open(outname, "w") as f:
    for line in locChanges:
        f.write(json.dumps(line))
        f.write("\n")

