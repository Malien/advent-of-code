from collections import Counter

file = open("input.txt")
contents = file.read().strip()
file.close()

c = Counter(map(int, contents.split(",")))

def poschange(a, b):
    r = abs(a - b)
    return int((1 + r) / 2 * r)

res = []
for root in range(min(c), max(c) + 1):
    s = 0
    for position, count in c.items():
        s += count * poschange(root, position)
    res.append(s)

print(min(res))


