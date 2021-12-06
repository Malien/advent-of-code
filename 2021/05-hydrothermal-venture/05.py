file = open("input.txt")
contents = file.read().strip()
file.close()

inputs = contents.split("\n")
res = []
for line in inputs:
    start, end = line.split(" -> ")
    x1,y1 = map(int, start.split(","))
    x2,y2 = map(int, end.split(","))
    res.append(((x1, y1), (x2, y2)))

maxsize = max(max(*start, *end) for start, end in res) + 1
mmap = [[0] * maxsize for _ in range(maxsize)]

def sign(x):
    if x > 0:
        return 1
    if x < 0:
        return -1
    return 0


for start, end in res:
    x1, y1 = start
    x2, y2 = end

    if x1 == x2 and y1 == y2:
        mmap[x1][y1] += 1
        continue

    if x1 == x2:
        if y1 > y2:
            y1, y2 = y2, y1
        for y in range(y1, y2 + 1):
            mmap[y][x1] += 1

    if y1 == y2:
        if x1 > x2:
            x1, x2 = x2, x1
        for x in range(x1, x2 + 1):
            mmap[y1][x] += 1

    dx, dy = x2 - x1, y2 - y1
    sx, sy = sign(dx), sign(dy)
    if abs(dx) == abs(dy):
        for i in range(abs(dx) + 1):
            mmap[y1 + i * sy][x1 + i * sx] += 1

count = 0
for row in mmap:
    for cell in row:
        if cell > 1:
            count += 1

print(count)

