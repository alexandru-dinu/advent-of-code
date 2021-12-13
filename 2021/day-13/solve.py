from __future__ import annotations

import re
import sys
from functools import reduce

import numpy as np


def fold_once(grid: np.ndarray, along: str, index: int) -> np.ndarray:
    """Fold `grid` `along` @ `index`."""

    if along == "y":
        return grid[:index, :] | np.flipud(grid[index + 1 :, :])

    if along == "x":
        return grid[:, :index] | np.fliplr(grid[:, index + 1 :])

    assert False, "unreachable"


def fold_all(grid: np.ndarray, inst: list[tuple[str, int]]) -> np.ndarray:
    return reduce(lambda g, i: fold_once(g, along=i[0], index=i[1]), inst, grid)


def show(grid: np.ndarray) -> None:
    # use space instead of "." for readability
    print("\n".join(map(lambda r: "".join(r), np.where(grid == 0, " ", "█"))))


def main():
    inst_re = re.compile(r"fold along (x|y)=(\d+)")

    with open(sys.argv[1]) as fp:
        coords, inst = fp.read().strip().split("\n\n")

    inst = map(lambda i: inst_re.match(i).groups(), inst.split("\n"))
    inst = [(c, int(d)) for c, d in inst]

    coords = map(lambda d: d.split(","), coords.split("\n"))
    coords = [(int(x), int(y)) for x, y in coords]

    xs, ys = zip(*coords)
    w = max(xs) + 1
    h = max(ys) + 1

    # dots  ('#') = True
    # empty ('.') = False
    grid = np.zeros((h, w), dtype=bool)
    grid[ys, xs] = True

    print("Part 1:", np.sum(fold_once(grid, *inst[0])))
    print("Part 2:")
    show(fold_all(grid, inst))


if __name__ == "__main__":
    main()
