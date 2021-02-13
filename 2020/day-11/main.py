import sys
from functools import partial

from functional import *


class CellType:
    EMPTY = "L"
    OCCUPIED = "#"
    FLOOR = "."


class Grid:
    def __init__(self, grid: list):
        self.__grid = grid
        self.rows = len(grid)
        self.cols = len(grid[0])

    def __getitem__(self, idx):
        if isinstance(idx, int):
            return self.__grid[idx]

        if isinstance(idx, tuple):
            i, j = idx
            return self.__grid[i][j]

    def __setitem__(self, idx, value):
        if isinstance(idx, int):
            self.__grid[idx] = value

        if isinstance(idx, tuple):
            i, j = idx
            self.__grid[i][j] = value

    def __eq__(self, other):
        return self.__grid == other.__grid

    def is_pos_valid(self, pos: tuple) -> bool:
        row, col = pos
        return 0 <= row < self.rows and 0 <= col < self.cols

    @property
    def num_occupied(self):
        return sum([x.count(CellType.OCCUPIED) for x in self.__grid])

    def copy(self):
        return Grid([[self[i][j] for j in range(self.cols)] for i in range(self.rows)])


# TODO: nicer rule repr?
def step(grid: Grid, occ_thr: int, count_occ: callable) -> Grid:
    new_grid = grid.copy()

    for i in range(grid.rows):
        for j in range(grid.cols):
            occ = count_occ(grid, i, j)

            if grid[i][j] == CellType.EMPTY and occ == 0:
                new_grid[i][j] = CellType.OCCUPIED

            elif grid[i][j] == CellType.OCCUPIED and occ >= occ_thr:
                new_grid[i][j] = CellType.EMPTY

            else:
                new_grid[i][j] = grid[i][j]

    return new_grid


def occ_small_fov(grid: Grid, i: int, j: int) -> list:
    xs = ((i + p, j + q) for p, q in cartesian([-1, 0, 1]) if not p == q == 0)
    xs = filter(lambda p: grid.is_pos_valid(p) and grid[p] == CellType.OCCUPIED, xs)

    return len(list(xs))


# TODO: more functional?
def occ_large_fov(grid: Grid, i: int, j: int) -> list:
    occ = 0
    pqs = [(p, q) for p, q in cartesian([-1, 0, 1]) if not p == q == 0]

    for p, q in pqs:
        k = 1
        while True:
            ip, jq = i + k * p, j + k * q

            if not grid.is_pos_valid((ip, jq)):
                break

            # don't go further first non-floor cells
            if grid[ip][jq] != CellType.FLOOR:
                occ += grid[ip][jq] == CellType.OCCUPIED
                break

            k += 1

    return occ


def get_final_grid(grid: Grid, step_func: callable) -> Grid:
    *_, last = until(no_change, iterate(step_func, grid))
    return last


if __name__ == "__main__":
    with open(sys.argv[1], "rt") as fp:
        grid = Grid([l.strip() for l in fp.readlines()])

    # :: Grid -> Grid
    step_p1 = partial(step, occ_thr=4, count_occ=occ_small_fov)
    print(f"Part 1: {get_final_grid(grid, step_p1).num_occupied}")

    # :: Grid -> Grid
    step_p2 = partial(step, occ_thr=5, count_occ=occ_large_fov)
    print(f"Part 1: {get_final_grid(grid, step_p2).num_occupied}")
