import re
import sys
import pickle
import numpy as np

from assemble import Tile, GridT, get_chararray, print_chararray, all_transforms


MONSTER = ["                  # ", "#    ##    ##    ###", " #  #  #  #  #  #   "]


def part1(grid: GridT) -> int:
    """
    Product of the 4 corner ids.
    """
    n = np.sqrt(len(grid)).astype(int)
    return np.product(
        [grid[0, 0].tid, grid[0, n - 1].tid, grid[n - 1, 0].tid, grid[n - 1, n - 1].tid]
    )


def to_binary(x: np.ndarray) -> np.ndarray:
    z = x.copy()
    z[z != "#"] = "0"
    z[z == "#"] = "1"
    return z.astype(int)


def convolve(x, k):
    kh, kw = k.shape
    xh, xw = x.shape

    out = np.zeros((xh - kh + 1, xw - kw + 1))

    for i in range(xh - kh + 1):
        for j in range(xw - kw + 1):
            patch = x[i : i + kh, j : j + kw]
            out[i, j] = (patch * k).sum()

    return out


def part2(grid: GridT) -> int:
    """
    Count '#' that are not part of a sea monster.
    """
    img = Tile(tid=0, mat=get_chararray(grid, with_borders=False))
    mask = to_binary(np.array([list(l) for l in MONSTER]))

    for t in all_transforms(img):
        x = to_binary(t.mat)
        c = convolve(x, mask)
        n = c[c == mask.sum()].size
        if n == 0:
            continue
        return x.sum() - n * mask.sum()

    assert False


if __name__ == "__main__":
    with open(sys.argv[1], "rb") as fp:
        grid: GridT = pickle.load(fp)

    # print_chararray(get_chararray(grid))
    print(f"Part 1: {part1(grid)}")
    print(f"Part 2: {part2(grid)}")
