from functools import partial
from pathlib import Path
from typing import TextIO

import numpy as np


def get_inp(fp):
    def read_grid(g: str):
        return np.array([[x == "#" for x in line] for line in g.split("\n")])

    return [read_grid(g) for g in fp.read().strip().split("\n\n")]


def find_axis_score(g: np.ndarray, diff: int) -> int:
    h, w = g.shape

    for i in range(1, w):
        m = min(i, w - i)
        if np.sum(np.fliplr(g[:, i - m : i]) ^ g[:, i : i + m]).sum() == diff:
            return i

    for i in range(1, h):
        m = min(i, h - i)
        if (np.flipud(g[i - m : i, :]) ^ g[i : i + m, :]).sum() == diff:
            return i * 100

    assert False


def solve(fp: TextIO):
    grids = get_inp(fp)

    p1 = sum(map(partial(find_axis_score, diff=0), grids))
    p2 = sum(map(partial(find_axis_score, diff=1), grids))

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 405
    assert p2 == 400


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
