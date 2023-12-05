import re
from collections import Counter
from functools import reduce
from itertools import starmap
from pathlib import Path
from typing import TextIO


def parse(card) -> tuple:
    win, sel = map(
        lambda xs: {int(x) for x in re.findall(r"\d+", xs)},
        card.split(":")[1].split(" | "),
    )
    return win, sel


def part1(win, sel):
    n = len(win & sel)
    return 0 if n == 0 else 2 ** (n - 1)


def part2(cards):
    tree = [Counter({i: 1}) for i in range(len(cards))]

    for i in reversed(range(len(cards))):
        win, sel = cards[i]
        n = len(win & sel)

        for j in range(i + 1, min(i + 1 + n, len(cards))):
            tree[i].update(tree[j])

    res = reduce(Counter.__add__, tree)

    # print({k+1: v for k, v in res.items()})
    return sum(res.values())


def solve(fp: TextIO) -> tuple[int, int]:
    cards = [parse(x.strip()) for x in fp]

    p1 = sum(starmap(part1, cards))
    p2 = part2(cards)

    return p1, p2


def test_example() -> None:
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 13
    assert p2 == 30


def main() -> None:
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
