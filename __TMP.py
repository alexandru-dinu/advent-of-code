###### 13
import re
from pathlib import Path

import numpy as np
from scipy.optimize import linprog


def solve_eq(eq: str, is_part_2):
    a, b, tgt = eq.split("\n")

    xa, ya = map(int, re.findall(r"\d+", a))
    xb, yb = map(int, re.findall(r"\d+", b))
    xtgt, ytgt = map(int, re.findall(r"\d+", tgt))

    A = np.array([[xa, xb], [ya, yb]])
    tgt = np.array([xtgt, ytgt])

    if is_part_2:
        tgt = tgt + 10000000000000

    c = np.array([3, 1])
    res = linprog(c, A_eq=A, b_eq=tgt)

    if res is None or res.x is None:
        return None

    x, y = res.x
    tot = res.fun

    # print(x, y, tot)

    # there is an integer solution
    if np.isclose(x, ix := round(x), rtol=0, atol=1e-3) and np.isclose(
        y, iy := round(y), rtol=0, atol=1e-3
    ):
        if is_part_2 or (ix <= 100 and iy <= 100):
            return ix, iy, round(tot)

    return None


def solve(fp):
    solns = [solve_eq(eq, is_part_2=True) for eq in fp.read().split("\n\n")]

    print(solns)
    p1 = sum(s[2] for s in solns if s is not None)
    p2 = ...

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == ...
    assert p2 == ...


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
###################################################################################
