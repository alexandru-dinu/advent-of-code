import sys
from typing import List, NewType

GridType = NewType('GridType', List[str])


class CellType:
    EMPTY    = 'L'
    OCCUPIED = '#'
    FLOOR    = '.'

def adjacent(grid: GridType, i: int, j: int) -> list:
    xs = []

    for p in [-1, 0, 1]:
        for q in [-1, 0, 1]:
            if p == 0 and q == 0:
                continue
            ip, jq = i + p, j + q
            if 0 <= ip < len(grid) and 0 <= jq < len(grid[0]):
                xs.append(grid[ip][jq])
    return xs


def step_p1(grid: GridType) -> GridType:
    new_grid = [[grid[i][j] for j in range(len(grid[0]))] for i in range(len(grid))]

    for i in range(len(grid)):
        for j in range(len(grid[0])):
            adj = adjacent(grid, i, j)

            if grid[i][j] == CellType.EMPTY:
                if adj.count(CellType.OCCUPIED) == 0:
                    new_grid[i][j] = CellType.OCCUPIED

            elif grid[i][j] == CellType.OCCUPIED:
                if adj.count(CellType.OCCUPIED) >= 4:
                    new_grid[i][j] = CellType.EMPTY

            else:
                new_grid[i][j] = grid[i][j]

        new_grid[i] = ''.join(new_grid[i])

    return new_grid


def step_p2(grid: GridType) -> GridType:
    pass


def stabilize(grid: GridType, step_func: callable) -> GridType:
    while True:
        new_grid = step_func(grid)
        if new_grid == grid:
            return grid
        grid = new_grid

    # unreachable
    assert False


def count_occupied(grid: GridType) -> int:
    return ''.join(grid).count(CellType.OCCUPIED)


if __name__ == "__main__":
    with open(sys.argv[1], 'rt') as fp:
        grid = [l.strip() for l in fp.readlines()]

    print(f'Part 1: {count_occupied(stabilize(grid, step_p1))}')
    # print(f'Part 1: {count_occupied(stabilize(grid, step_p2))}')
