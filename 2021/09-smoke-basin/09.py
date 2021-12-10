file = open("input.txt")
contents = file.read().strip()
file.close()

inp = contents.split("\n")

smallests = []
basins = [[False] * len(row) for row in inp]

def dfs(i, j):
    if i < 0 or i >= len(inp):
        return 0
    if j < 0 or j >= len(inp[0]):
        return 0
    if inp[i][j] == "9":
        return 0
    if basins[i][j]: 
        return 0

    basins[i][j] = True
    return 1 + dfs(i-1, j) + dfs(i+1, j) + dfs(i, j-1) + dfs(i, j+1)


sizes = []
for i, row in enumerate(inp):
    for j, cell in enumerate(row):
        if i > 0 and cell >= inp[i-1][j]:
            continue
        if i < len(inp) - 1 and cell >= inp[i+1][j]:
            continue
        if j > 0 and cell >= inp[i][j-1]:
            continue
        if j < len(row) - 1 and cell >= inp[i][j+1]:
            continue
        print(i, j, cell)
        sizes.append(dfs(i, j))

sizes.sort()
res = 1
for i in sizes[-3:]:
    res *= i
print(res)


