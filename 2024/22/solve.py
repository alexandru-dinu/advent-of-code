from collections import defaultdict
from pathlib import Path
from typing import TextIO

import numpy as np

MOD = 1 << 24


def f(x):
    # x % pow2 == x & (pow2 - 1)
    x = ((x << 6) ^ x) & (MOD - 1)
    x = ((x >> 5) ^ x) & (MOD - 1)
    x = ((x << 11) ^ x) & (MOD - 1)
    return x


def iterate(x, n):
    return [(x := f(x)) for _ in range(n)]


def solve(fp: TextIO):
    secrets = [int(x) for x in fp.read().strip().split("\n")]

    prices = np.array([[s, *iterate(s, 2000)] for s in secrets])
    prices_ones = prices % 10
    diffs = np.diff(prices_ones, axis=1)

    chg2buy2val = defaultdict(dict)
    for b, diff in enumerate(diffs):
        for w in range(4, len(diff) + 1):
            chg = tuple(diff[w - 4 : w])
            # stop at first `chg` for buyer `b`
            if b not in chg2buy2val[chg]:
                chg2buy2val[chg][b] = prices_ones[b, w]

    p1 = prices[:, -1].sum()
    p2, chg = max((sum(buy2val.values()), chg) for chg, buy2val in chg2buy2val.items())

    return p1, p2


def test_example():
    assert iterate(123, 10) == [
        15887950,
        16495136,
        527345,
        704524,
        1553684,
        12683156,
        11100544,
        12249484,
        7753432,
        5908254,
    ]

    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)
        assert p1 == 37327623
        assert p2 == 24

    with open(Path(__file__).parent / "example2") as fp:
        p1, p2 = solve(fp)
        assert p1 == 37990510
        assert p2 == 23


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
