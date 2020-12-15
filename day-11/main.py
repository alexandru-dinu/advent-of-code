import sys
from itertools import product, accumulate, repeat
from functools import partial
from typing import List, NewType

class CellType:
    EMPTY    = 'L'
    OCCUPIED = '#'
    FLOOR    = '.'

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
        return 0 <= row < self.rows and \
               0 <= col < self.cols

    @property
    def num_occupied(self):
        return sum([x.count(CellType.OCCUPIED) for x in self.__grid])

    def copy(self):
        return Grid([
            [self[i][j] for j in range(self.cols)]
            for i in range(self.rows)
        ])


# TODO: nicer rule repr?
def step(grid: Grid, occ_thr: int, get_nearby: callable) -> Grid:
    new_grid = grid.copy()

    for i in range(grid.rows):
        for j in range(grid.cols):
            occ = get_nearby(grid, i, j).count(CellType.OCCUPIED)

            if grid[i][j] == CellType.EMPTY and occ == 0:
                    new_grid[i][j] = CellType.OCCUPIED
            elif grid[i][j] == CellType.OCCUPIED and occ >= occ_thr:
                    new_grid[i][j] = CellType.EMPTY
            else:
                new_grid[i][j] = grid[i][j]

    return new_grid


def smaller_fov(grid: Grid, i: int, j: int) -> list:
    pos = (
        (i + p, j + q) for p, q in product([-1, 0, 1], repeat=2) if not p == q == 0
    )
    return [grid[p][q] for p, q in filter(grid.is_pos_valid, pos)]


# TODO: nicer?
def larger_fov(grid: Grid, i: int, j: int) -> list:
    xs = []

    pos = [(p, q) for p, q in product([-1, 0, 1], repeat=2) if not p == q == 0]

    for p, q in pos:
        k = 1

        while True:
            ip, jq = i + k * p, j + k * q

            if not grid.is_pos_valid((ip, jq)):
                break

            # stop at first non-floor
            if grid[ip][jq] != CellType.FLOOR:
                xs.append(grid[ip][jq])
                break

            k += 1

    return xs


def until_equilibrium(it):
    def __no_repeat(prev, curr):
        if prev == curr:
            raise StopIteration
        else:
            return curr
    return accumulate(it, __no_repeat)


def iterate(f, x):
    return accumulate(repeat(x), lambda fx, _: f(fx))


def stabilize(grid: Grid, step_func: callable) -> Grid:
    *_, last = until_equilibrium(iterate(step_func, grid))
    return last


if __name__ == "__main__":
    with open(sys.argv[1], 'rt') as fp:
        grid = Grid([l.strip() for l in fp.readlines()])

    # :: Grid -> Grid
    step_p1 = partial(step, occ_thr=4, get_nearby=smaller_fov)
    print(f'Part 1: {stabilize(grid, step_p1).num_occupied}')

    # :: Grid -> Grid
    step_p2 = partial(step, occ_thr=5, get_nearby=larger_fov)
    print(f'Part 1: {stabilize(grid, step_p2).num_occupied}')