from pathlib import Path
from typing import TextIO

import numpy as np


def is_safe(xs) -> bool:
    a = np.sign(xs[0]) == np.sign(xs)
    b = np.abs(xs) >= 1
    c = np.abs(xs) <= 3
    return (1 - (a * b * c)).sum() == 0


def try_safe(xs) -> bool:
    if is_safe(np.diff(xs)):
        return True

    for i in range(len(xs)):
        if is_safe(np.diff(xs[:i] + xs[i + 1 :])):
            return True

    return False


def solve(fp: TextIO):
    xss = [list(map(int, line.split())) for line in fp.readlines()]

    p1 = sum(map(lambda xs: is_safe(np.diff(xs)), xss))
    print("---")
    p2 = sum(map(try_safe, xss))

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 2
    assert p2 == 4


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
