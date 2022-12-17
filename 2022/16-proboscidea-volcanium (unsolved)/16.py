from collections import namedtuple, defaultdict
import re

# parseValve line = Room { name = (a,b), flowRate = read flowRate, tunnels }
#   where (a:b:_) = drop 6 line
#         flowRate = takeWhile (/=';') $ drop 23 line
#         tunnels = parseTunnels $ dropWhile (/=';') line

# parseTunnels line | isPrefixOf "; tunnels lead to valves " line = map toPair $ splitOn ", " $ drop 25 line
#                   | isPrefixOf "; tunnel leads to valve " line = [toPair $ drop 24 line]


inp = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"""

inp = open("in").read()

inp = filter(lambda x: x, inp.split("\n"))

Room = namedtuple("Room", ("name", "flow_rate", "tunnels"))

rooms = {}

for line in inp:
    name = line[6:8]
    flow_rate = int(re.search("\\d+", line).group())
    _, tun = line.split(";")
    tunnels = re.findall("[A-Z]+", tun)
    rooms[name] = Room(name, flow_rate, tuple(tunnels))

zero_valves = { x.name for x in rooms.values() if x.flow_rate == 0 }

Traversal = namedtuple(
    "Traversal", ("node", "concurrent_flow_rate", "pressure_relieved", "time_left", "valves_opened", "path")
)

queue = [
    Traversal(
        node = "AA",
        concurrent_flow_rate = 0,
        pressure_relieved = 0,
        time_left = 30,
        valves_opened = zero_valves,
        path = []
    )
]

previous_visits = defaultdict(int)
max_pressure = 0

def potential_pressure(t):
    return t.pressure_relieved + t.concurrent_flow_rate * t.time_left

prev_len = 0

while queue:
    # input()
    if abs(prev_len - len(queue)) > 500:
        print(len(queue))
        prev_len = len(queue)
    t = queue[0]
    queue = queue[1:]
    # print(t)
    
    if t.time_left == 0 or len(rooms) == len(t.valves_opened):
        # print(t.pressure_relieved)
        max_pressure = max(max_pressure, potential_pressure(t))
        continue
    if potential_pressure(t) < previous_visits[t.node]:
        # print("Evicted")
        continue
    previous_visits[t.node] = potential_pressure(t)
    if not (t.node in t.valves_opened):
        new_set = set(t.valves_opened)
        new_set.add(t.node)
        queue.append(Traversal(
            node = t.node, 
            concurrent_flow_rate = t.concurrent_flow_rate + rooms[t.node].flow_rate,
            pressure_relieved = t.pressure_relieved + t.concurrent_flow_rate,
            time_left = t.time_left - 1,
            valves_opened = new_set,
            path = t.path + ["open"]
        ))
    # print("Neighbors: ", rooms[t.node].tunnels)
    for neighbor in rooms[t.node].tunnels:
        queue.append(Traversal(
            node = neighbor,
            concurrent_flow_rate = t.concurrent_flow_rate,
            pressure_relieved = t.pressure_relieved + t.concurrent_flow_rate,
            time_left = t.time_left - 1,
            valves_opened = t.valves_opened,
            path = t.path + [f"move {neighbor}"]
        ))

print(max_pressure)

