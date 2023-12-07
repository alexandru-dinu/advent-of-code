from collections import Counter
from functools import cmp_to_key
from pathlib import Path
from typing import TextIO

HANDS = {
    "5kind": lambda h: len(set(h)) == 1,
    "4kind": lambda h: sorted(Counter(h).values()) == [1, 4],
    "fh": lambda h: sorted(Counter(h).values()) == [2, 3],
    "3kind": lambda h: sorted(Counter(h).values()) == [1, 1, 3],
    "2pair": lambda h: sorted(Counter(h).values()) == [1, 2, 2],
    "1pair": lambda h: sorted(Counter(h).values()) == [1, 1, 1, 2],
    "high": lambda h: len(Counter(h).values()) == 5,
}
ORDER = list(reversed(HANDS.keys()))


class Deck1:
    CARDS = "23456789TJQKA"

    @staticmethod
    def type_of(h: str):
        for t, f in HANDS.items():
            if f(h):
                return t


class Deck2:
    CARDS = "J" + Deck1.CARDS

    @staticmethod
    def type_of(h: str):
        for t, f in HANDS.items():
            if "J" in h:
                for c in reversed(Deck1.CARDS):
                    if f(h.replace("J", c)):
                        return t
            elif f(h):
                return t


def cmp(deck):
    def _inner(x: tuple, y: tuple):
        h1, _ = x
        h2, _ = y

        if h1 == h2:
            return 0

        t1, t2 = map(deck.type_of, (h1, h2))

        if t1 != t2:
            return ORDER.index(t1) - ORDER.index(t2)

        for c1, c2 in zip(list(h1), list(h2)):
            if c1 == c2:
                continue
            return deck.CARDS.index(c1) - deck.CARDS.index(c2)

    return _inner


def get_inp(fp: TextIO) -> tuple:
    hs, bs = zip(*map(lambda l: l.split(), fp.read().strip().split("\n")))
    return hs, list(map(int, bs))


def solve(hs, bs, deck) -> tuple[int, int]:
    return sum(
        r * b
        for r, (_, b) in enumerate(
            sorted(zip(hs, bs), key=cmp_to_key(cmp(deck))), start=1
        )
    )


def test_example() -> None:
    with open(Path(__file__).parent / "example") as fp:
        hs, bs = get_inp(fp)
        assert solve(hs, bs, Deck1) == 6440
        assert solve(hs, bs, Deck2) == 5905


def main() -> None:
    with open(0) as fp:
        hs, bs = get_inp(fp)

    print(f"Part 1: {solve(hs, bs, Deck1)}")
    print(f"Part 2: {solve(hs, bs, Deck2)}")


if __name__ == "__main__":
    main()
