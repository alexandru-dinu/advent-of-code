from collections import Counter
from pathlib import Path
from typing import TextIO


def solve(fp: TextIO):
    l, r = zip(*[x.strip().split() for x in fp.readlines()])
    l = list(map(int, l))
    r = list(map(int, r))

    p1 = sum(abs(i - j) for i, j in zip(sorted(l), sorted(r)))

    cnt = Counter(r)
    p2 = sum(i * cnt[i] for i in l)

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 11
    assert p2 == 31


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
