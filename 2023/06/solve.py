import re
from math import prod
from pathlib import Path
from typing import TextIO

from sympy import S, solveset, symbols

X = symbols("X")


def ineq(t: int, d: int) -> int:
    """
    Inequality to solve: x * (t - x) > d
    where x is the amount of time spent charging the boat.
    """
    return solveset(X * (t - X) > d, X, domain=S.Integers).__len__()


def solve(fp: TextIO) -> tuple[int, int]:
    ts, ds = map(
        lambda xs: [int(x) for x in re.findall(r"\d+", xs)],
        fp.read().strip().split("\n"),
    )

    p1 = prod([ineq(t, d) for t, d in zip(ts, ds)])
    p2 = ineq(*map(lambda xs: int("".join(map(str, xs))), (ts, ds)))

    return p1, p2


def test_example() -> None:
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 288
    assert p2 == 71503


def main() -> None:
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
