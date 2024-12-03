import re
from pathlib import Path
from typing import TextIO


def solve(fp: TextIO):
    x = fp.read().strip()

    p1 = 0
    p2 = 0
    ok = True
    for m in re.finditer(r"(mul\((\d{1,3}),(\d{1,3})\))|(do\(\))|(don't\(\))", x):
        mul, a, b, do, dont = m.groups()
        if mul is not None:
            p1 += int(a) * int(b)
            p2 += int(a) * int(b) * ok
        if do is not None:
            ok = True
        if dont is not None:
            ok = False

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        p1, _ = solve(fp)

    with open(Path(__file__).parent / "example2") as fp:
        _, p2 = solve(fp)

    assert p1 == 161
    assert p2 == 48


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
