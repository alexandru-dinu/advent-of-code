import sys

import numpy as np


def get_inp(fname: str):
    return np.loadtxt(fname, dtype=int)


def iterate(f, x):
    yield x
    yield from iterate(f, f(x))


def fwd(row: np.ndarray):
    g = iterate(np.diff, row)
    x = 0

    while True:
        d = next(g)
        x += d[-1]
        if (d == d[0]).all():
            return x


def bwd(row: np.ndarray):
    g = iterate(np.diff, row)

    def inner():
        d = next(g)
        if (d == d[0]).all():
            return d[0]
        return d[0] - inner()

    return inner()


def solve(mat: np.ndarray):
    p1 = sum(map(fwd, mat))
    p2 = sum(map(bwd, mat))

    return p1, p2


def test_example():
    assert solve(get_inp("example")) == (114, 2)


def main():
    p1, p2 = solve(get_inp(sys.argv[1]))

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
