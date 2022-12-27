import os
import time
from copy import deepcopy

import numpy as np


class Grid:
    def __init__(self, mat: np.ndarray):
        self.mat = mat

    @property
    def shape(self):
        return self.mat.shape

    def _check_bounds(self, i, j):
        h, w = self.shape
        if not (0 <= i < h and 0 <= j < w):
            raise IndexError(i, j)

    def __getitem__(self, key):
        assert isinstance(key, tuple)
        i, j = key
        self._check_bounds(i, j)
        return self.mat[i, j]

    def __setitem__(self, key, value):
        assert isinstance(key, tuple)
        i, j = key
        self._check_bounds(i, j)
        self.mat[i, j] = value

    def __str__(self):
        x = np.array([".", "#", "o", "?"])
        return "\n".join(map(lambda row: "".join(x[row]), self.mat))

    __repr__ = __str__


def parse_input(lines):
    obs = [
        [tuple(map(int, x.split(","))) for x in line.split(" -> ")] for line in lines
    ]
    dx_min = min(min(x for x, _ in row) for row in obs)
    dx_max = max(max(x for x, _ in row) for row in obs)
    dy_max = max(max(y for _, y in row) for row in obs)

    # create the grid
    w = dx_max - dx_min + 1
    h = dy_max + 1
    mat = np.zeros((h, w), dtype=np.int8)

    # repr the obstacles
    for row in obs:
        for (sx, sy), (ex, ey) in zip(row[:-1], row[1:]):
            x1, x2 = sorted([sx - dx_min, ex - dx_min])
            y1, y2 = sorted([sy, ey])
            mat[y1 : y2 + 1, x1 : x2 + 1] = 1

    # also return the source of the sand
    return Grid(mat), (0, 500 - dx_min)


def simulate_part1(grid, si, sj, verbose=False):
    count = 0
    fall = False

    while not fall:
        i, j = si, sj  # start at source
        rest = False  # per unit of sand

        while not rest and not fall:
            try:
                for dj in [0, -1, 1]:
                    if grid[i + 1, j + dj] == 0:  # can fall
                        i += 1
                        j += dj
                        break
                else:
                    rest = True
                    grid[i, j] = 2
                    count += 1
            except IndexError:
                fall = True

        if verbose:
            print(grid)
            time.sleep(0.5)
            os.system("clear")

    return count


def simulate_part2(grid, si, sj, verbose=False):
    # assume the floor is an infinite horizontal line
    # new y = 2 + the highest y coordinate of any point in the scan
    grid.mat = np.pad(grid.mat, ((0, 2), (0, 0)), mode="constant", constant_values=0)
    grid.mat[-1, :] = 1

    count = 0
    blocked = False

    while not blocked:
        i, j = si, sj  # start at source
        rest = False  # per unit of sand

        while not rest and not blocked:
            try:
                for dj in [0, -1, 1]:
                    if grid[i + 1, j + dj] == 0:  # can fall
                        i += 1
                        j += dj
                        break
                else:
                    rest = True
                    grid[i, j] = 2
                    count += 1
            except IndexError as e:
                ei, ej = e.args

                if ei == grid.shape[0]:
                    rest = True
                    grid[ei - 2, ej] = 2
                    count += 1
                elif ej < 0:
                    sj += 1
                    i, j = si, sj
                    grid.mat = np.pad(
                        grid.mat,
                        ((0, 0), (1, 0)),
                        mode="constant",
                        constant_values=0,
                    )
                    grid.mat[-1, :] = 1

                elif ej >= grid.shape[1]:
                    i, j = si, sj
                    grid.mat = np.pad(
                        grid.mat,
                        ((0, 0), (0, 1)),
                        mode="constant",
                        constant_values=0,
                    )
                    grid.mat[-1, :] = 1

                else:
                    assert False, e

        if (i, j) == (si, sj):
            blocked = True

        if verbose:
            print(grid)
            time.sleep(0.2)
            os.system("clear")

    return count


def main():
    with open(0) as fp:
        xs = [x.strip() for x in fp]

    grid, (si, sj) = parse_input(xs)

    print("Part 1:", simulate_part1(deepcopy(grid), si, sj, verbose=False))
    print("Part 2:", simulate_part2(deepcopy(grid), si, sj, verbose=False))


if __name__ == "__main__":
    main()
