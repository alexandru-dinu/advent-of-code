import re
from itertools import starmap
from pathlib import Path
from typing import TextIO

import numpy as np


def is_valid(a, b, c) -> bool:
    return a < (b + c) and b < (a + c) and c < (a + b)


def solve(fp: TextIO) -> tuple[int, int]:
    content = fp.read().strip()

    nums = np.array(list(map(int, re.findall(r"\d+", content))))
    assert len(nums) % 3 == 0

    p1 = sum(starmap(is_valid, nums.reshape(-1, 3)))
    p2 = sum(starmap(is_valid, nums.reshape(-1, 3).T.reshape(-1, 3)))

    return p1, p2


def test_example() -> None:
    assert not is_valid(5, 10, 25)
    assert is_valid(3, 4, 5)

    with open(Path(__file__).parent / "example") as fp:
        assert solve(fp) == (3, 6)


def main() -> None:
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
