import itertools as it
from functools import reduce
from pathlib import Path
from typing import TextIO

import numpy as np


def get_inp(fp: TextIO):
    return np.array([[x == "#" for x in line.strip()] for line in fp]).astype(bool)


def manh(p1, p2):
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])


def shortest_dist(mat: np.ndarray, repeats: int) -> int:
    empty_cols = set(np.argwhere(mat.sum(axis=0) == 0).reshape(-1))
    empty_rows = set(np.argwhere(mat.sum(axis=1) == 0).reshape(-1))

    extra = lambda needles, haystack: sum(i in haystack for i in needles)
    mk_range = lambda p1, p2, a: range(min(p1[a], p2[a]) + 1, max(p1[a], p2[a]))

    return reduce(
        lambda acc, ps: acc
        + (
            manh(*ps)
            + (repeats - 1) * extra(empty_rows, mk_range(*ps, 0))
            + (repeats - 1) * extra(empty_cols, mk_range(*ps, 1))
        ),
        it.combinations(np.argwhere(mat == 1), 2),
        0,
    )


def solve(fp: TextIO):
    mat = get_inp(fp)

    p1 = shortest_dist(mat, repeats=2)
    p2 = shortest_dist(mat, repeats=1_000_000)

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        mat = get_inp(fp)

    assert shortest_dist(mat, repeats=2) == 374
    assert shortest_dist(mat, repeats=10) == 1030
    assert shortest_dist(mat, repeats=100) == 8410


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
