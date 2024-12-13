import re
from pathlib import Path

import numpy as np


def solve_eq(eq: str, is_part_2):
    a, b, tgt = eq.split("\n")

    xa, ya = map(int, re.findall(r"\d+", a))
    xb, yb = map(int, re.findall(r"\d+", b))
    xtgt, ytgt = map(int, re.findall(r"\d+", tgt))

    A = np.array([[xa, xb], [ya, yb]])
    tgt = np.array([xtgt, ytgt])

    if is_part_2:
        tgt += 10000000000000

    x, y = np.linalg.inv(A) @ tgt
    tot = 3 * x + y

    # integer solution
    if (
        np.isclose(x, ix := round(x), rtol=0, atol=1e-3)
        and np.isclose(y, iy := round(y), rtol=0, atol=1e-3)
        and ((ix <= 100 and iy <= 100) or is_part_2)
    ):
        return ix, iy, round(tot)

    return None


def solve(fp):
    eqs = fp.read().strip().split("\n\n")

    p1 = p2 = 0
    for eq in eqs:
        if (s := solve_eq(eq, is_part_2=False)) is not None:
            p1 += s[2]
        if (s := solve_eq(eq, is_part_2=True)) is not None:
            p2 += s[2]

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 480
    assert p2 == 875318608908


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
