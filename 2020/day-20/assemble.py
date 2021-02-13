"""
"candidates -> valid -> backtrack" idea from
https://github.com/grey-area/aoc2020/blob/master/Day20/Day20_1.scala
"""

import sys
import pickle
import numpy as np
from typing import Dict, NewType, Set, Tuple
from dataclasses import dataclass
from itertools import chain


@dataclass
class Tile:
    tid: int
    mat: np.ndarray

    def __hash__(self):
        return hash((hash(self.tid), hash(str(self.mat))))

    def __eq__(self, other):
        return (self.tid == other.tid) and (self.mat == other.mat).all()

    def __repr__(self):
        _id = f"<{self.tid}>"
        _mat = "\n".join(map(lambda s: "".join(s), self.mat))
        return f"{_id}\n{_mat}"

    __str__ = __repr__

    @property
    def top(self) -> np.ndarray:
        return self.mat[0, :]

    @property
    def bottom(self) -> np.ndarray:
        return self.mat[-1, :]

    @property
    def left(self) -> np.ndarray:
        return self.mat[:, 0]

    @property
    def right(self) -> np.ndarray:
        return self.mat[:, -1]

    def fliplr(self):
        return Tile(self.tid, np.fliplr(self.mat))

    def flipud(self):
        return Tile(self.tid, np.flipud(self.mat))

    def rot(self, k):
        return Tile(self.tid, np.rot90(self.mat, k=k))


GridT = NewType("GridT", Dict[Tuple[int, int], Tile])


def all_transforms(tile: Tile) -> Set[Tile]:
    f = tile.fliplr()
    return [*[tile.rot(k) for k in range(4)], *[f.rot(k) for k in range(4)]]


def get_chararray(acc: GridT, with_borders=True) -> np.ndarray:
    size = np.sqrt(len(acc)).astype(int)
    n = 10 if with_borders else 8
    grid = np.chararray(shape=(n * size, n * size))

    for i in range(size):
        for j in range(size):
            tile = acc[i, j].mat
            if not with_borders:
                tile = tile[1:-1, 1:-1]

            grid[i * n : (i + 1) * n, j * n : (j + 1) * n] = tile

    return grid.decode("utf-8")


def print_chararray(grid: np.ndarray, with_borders=True) -> None:
    n, _ = grid.shape
    m = 10 if with_borders else 8

    for i in range(n):
        for j in range(n):
            print(grid[i, j], end="")
            if j % m == m - 1:
                print(" ", end="")
        if i % m == m - 1:
            print()
        print()


def assemble(tiles: Dict[int, Tile]) -> np.ndarray:
    def _inner(rest: Set[int], acc: GridT) -> bool:
        if rest == set():
            return True

        i, j = len(acc) // SIZE, len(acc) % SIZE

        is_valid = lambda tile: (
            (i == 0) or (tile.top == acc[i - 1, j].bottom).all()
        ) and ((j == 0) or (tile.left == acc[i, j - 1].right).all())

        for tile in filter(
            is_valid, chain(*[all_transforms(tiles[tid]) for tid in rest])
        ):
            acc[i, j] = tile
            if _inner(rest - {tile.tid}, acc):
                return True
            del acc[i, j]

        return False

    SIZE = np.sqrt(len(tiles)).astype(int)
    acc = {}
    assert _inner(rest=set(tiles.keys()), acc=acc), "No solution found!"
    return acc


def read_input(file_path: str) -> Dict[int, Tile]:
    with open(file_path, "rt") as fp:
        xs = fp.read().strip().split("\n\n")
        xs = [x.split() for x in xs]
        xs = [(int(x[1].strip(":")), x[2:]) for x in xs]

    return {tid: Tile(tid=tid, mat=np.array([*map(list, raw)])) for tid, raw in xs}


if __name__ == "__main__":
    tiles = read_input(sys.argv[1])

    grid = assemble(tiles)
    print_chararray(get_chararray(grid))

    suffix = sys.argv[1].split("/")[-1].split(".")[0]
    path = f"./input/grid_{suffix}.pkl"
    with open(path, "wb") as fp:
        pickle.dump(grid, fp)
    print(f"dumped grid to {path}")
