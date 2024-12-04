from pathlib import Path
from typing import TextIO

import numpy as np


def check_xmas(xss: np.ndarray):
    pat = list("XMAS")
    n, m = xss.shape
    k = len(pat)
    tot = 0

    # left-right & up-down (2)
    for xs in np.vstack((xss, np.transpose(xss))):
        for i in range(m - k + 1):
            tot += (xs[i : i + k] == pat).all()
            tot += (xs[i : i + k] == pat[::-1]).all()

    # diagonals (4)
    for i in range(n - k + 1):
        for j in range(m - k + 1):
            cur = xss[i : i + k, j : j + k]

            tot += (np.diag(cur) == pat).all()
            tot += (np.diag(cur) == pat[::-1]).all()
            tot += (np.diag(np.fliplr(cur)) == pat).all()
            tot += (np.diag(np.fliplr(cur)) == pat[::-1]).all()

    return tot


def check_x_mas(xss: np.ndarray):
    def f(x):
        # x w/o middle
        return np.array([x[0], x[2]])

    pat = ["M", "S"]
    n, m = xss.shape
    k = 3
    tot = 0

    for i in range(n - k + 1):
        for j in range(m - k + 1):
            cur = xss[i : i + k, j : j + k]

            d1 = np.diag(cur)
            d2 = np.diag(np.fliplr(cur))

            tot += (
                cur[1, 1] == "A"
                and ((f(d1) == pat).all() or (f(d1) == pat[::-1]).all())
                and ((f(d2) == pat).all() or (f(d2) == pat[::-1]).all())
            )

    return tot


def solve(fp: TextIO):
    xss = np.array([list(xs.strip()) for xs in fp.readlines()])

    p1 = check_xmas(xss)
    p2 = check_x_mas(xss)

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        p1, _ = solve(fp)
    with open(Path(__file__).parent / "example2") as fp:
        _, p2 = solve(fp)

    assert p1 == 18
    assert p2 == 9


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
