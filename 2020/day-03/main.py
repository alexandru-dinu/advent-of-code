import sys
from typing import List
from functools import reduce

TREE = "#"
SPACE = "."


def count(grid: List[str], slope: tuple) -> int:
    rows, cols = len(grid), len(grid[0])
    dx, dy = slope

    total = 0
    x, y = dx, dy

    while y < rows:
        total += grid[y][x] == TREE
        y, x = y + dy, (x + dx) % cols

    return total


if __name__ == "__main__":
    with open(sys.argv[1], "rt") as fp:
        grid = [l.strip() for l in fp.readlines()]

    slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    ans = [count(grid, slope) for slope in slopes]

    for (sX, sY), a in zip(slopes, ans):
        print(f"right {sX}, down {sY}: {a}")

    print(reduce(lambda x, y: x * y, ans, 1))
