from collections import Counter
from functools import reduce
from itertools import combinations
from operator import add
from pathlib import Path
from typing import TextIO


def get_inp(fp: TextIO) -> list[str]:
    return [x.strip() for x in fp]


def part1(ids):
    def check(s: str) -> complex:
        cnt = Counter(s).values()
        return complex(2 in cnt, 3 in cnt)

    z = reduce(add, map(check, ids), 0)
    return int(z.real * z.imag)


def part2(ids):
    def diff(x: str, y: str) -> int:
        assert len(x) == len(y)
        d = None

        for i in range(len(x)):
            if x[i] != y[i]:
                if d is None:
                    d = i
                else:
                    return None

        assert d is not None
        return d

    for x, y in combinations(ids, 2):
        if (d := diff(x, y)) is not None:
            return x[:d] + x[d + 1 :]


def test_example() -> None:
    with open(Path(__file__).parent / "example1") as fp:
        assert part1(get_inp(fp)) == 12

    with open(Path(__file__).parent / "example2") as fp:
        assert part2(get_inp(fp)) == "fgij"


def main() -> None:
    with open(0) as fp:
        ids = get_inp(fp)
        p1, p2 = part1(ids), part2(ids)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
