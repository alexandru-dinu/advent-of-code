import itertools
import sys
from pathlib import Path
from typing import TextIO

import numpy as np

sys.setrecursionlimit(999999)


def get_inp(fp: TextIO):
    return np.array([list(x.strip()) for x in fp])


def neighbours(grid: np.ndarray, c):
    def inner(grid, ci, cj):
        match grid[ci, cj]:
            case "|":
                yield (ci + 1, cj)
                yield (ci - 1, cj)
            case "-":
                yield (ci, cj + 1)
                yield (ci, cj - 1)
            case "L":
                yield (ci - 1, cj)
                yield (ci, cj + 1)
            case "J":
                yield (ci - 1, cj)
                yield (ci, cj - 1)
            case "7":
                yield (ci + 1, cj)
                yield (ci, cj - 1)
            case "F":
                yield (ci, cj + 1)
                yield (ci + 1, cj)
            case "." | "S":
                yield (ci - 1, cj)
                yield (ci + 1, cj)
                yield (ci, cj - 1)
                yield (ci, cj + 1)
            case _:
                assert False, "unreachable"

    h, w = grid.shape
    ci, cj = c

    for ni, nj in inner(grid, ci, cj):
        if not (0 <= ni < h and 0 <= nj < w) or grid[ni, nj] == ".":
            continue
        yield ni, nj


def solve(grid: np.ndarray):
    start = tuple(np.argwhere(grid == "S")[0])

    vis = {start}
    par = {start: None}
    loop = []

    def inner(cur):
        for n in neighbours(grid, cur):
            if n not in vis:
                par[n] = cur
                vis.add(n)
                if inner(n):
                    return True
            elif n != par[cur]:
                assert n == start and par[n] is None, (n, cur, par[cur])

                x = cur
                while x is not None:
                    loop.append(x)
                    x = par[x]

                return True

        return False

    assert inner(start)

    # https://en.wikipedia.org/wiki/Shoelace_formula
    shoelace = sum(
        (a[0] * b[1]) - (b[0] * a[1]) for a, b in itertools.pairwise([*loop, loop[0]])
    )
    # https://en.wikipedia.org/wiki/Pick%27s_theorem
    inside = 1 + (abs(shoelace) - len(loop)) // 2

    p1 = len(loop) // 2
    p2 = inside

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        assert solve(get_inp(fp)) == (23, 4)


def main():
    with open(0) as fp:
        p1, p2 = solve(get_inp(fp))

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
