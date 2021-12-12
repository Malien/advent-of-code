f = open("input.txt")
file = f.read().strip()
f.close()

connections = {}

for line in file.split("\n"):
    node1, node2 = line.split("-")
    if node1 in connections:
        connections[node1].append(node2)
    else:
        connections[node1] = [node2]
    if node2 in connections:
        connections[node2].append(node1)
    else:
        connections[node2] = [node1]

def traversePaths(pathBefore, visited, visitedTwice = None):
    last_node = pathBefore[-1]
    if last_node == "end":
        return [pathBefore]
    new_visited = set(visited)
    if last_node.islower():
        new_visited.add(last_node)

    next_paths = []
    for next_node in connections[last_node]:
        if not next_node in visited:
            new_path = pathBefore[:]
            new_path.append(next_node)
            next_paths.extend(traversePaths(new_path, new_visited, visitedTwice))
        if not visitedTwice and next_node in visited and next_node != "start":
            new_path = pathBefore[:]
            new_path.append(next_node)
            next_paths.extend(traversePaths(new_path, new_visited, next_node))

    return next_paths
            
res = traversePaths(["start"], set(["start"]))
print(len(res))

