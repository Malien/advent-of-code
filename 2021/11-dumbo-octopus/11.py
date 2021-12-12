import numpy as np

file = open("input.txt")
contents = file.read().strip()
file.close()

grid = [[int(x) for x in row] for row in contents.split("\n")]

def neighbours(i, j):
    yield i+1, j
    yield i+1, j+1
    yield i+1, j-1
    yield i  , j+1
    yield i  , j-1
    yield i-1, j+1
    yield i-1, j
    yield i-1, j-1

def flash(grid, flashed, i, j):
    if i < 0:
        return 0
    if i >= len(grid):
        return 0
    if j < 0:
        return 0
    if j >= len(grid):
        return 0
    if flashed[i][j]:
        return 0

    grid[i][j] += 1
    if grid[i][j] > 9:
        flashed[i][j] = True
        grid[i][j] = 0
        total = 1
        for ni, nj in neighbours(i, j):
            total += flash(grid, flashed, ni, nj)
        return total
    return 0

day = 1
while True:
    flashes = 0
    flashed = [[False for _ in row] for row in grid]
    for i, row in enumerate(grid):
        for j, cell in enumerate(grid):
            flashes += flash(grid, flashed, i, j)
    if flashes == len(grid) * len(grid[0]):
        print(day)
        break
    day += 1

