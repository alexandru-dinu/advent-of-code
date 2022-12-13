import json
from enum import Enum
from itertools import chain, starmap, zip_longest
from typing import Any

import numpy as np


class Ord(Enum):
    LT = -1
    EQ = 0
    GT = 1


def promote(x):
    return [x] if isinstance(x, int) else x


def compare(x, y) -> Ord:
    tx, ty = type(x), type(y)

    # (None, *) -> LT
    # (*, None) -> GT
    if None in (x, y):
        return Ord((y is None) - (x is None))

    if tx == ty == int:
        return Ord(np.sign(x - y))

    if tx == ty == list:
        for a, b in zip_longest(x, y, fillvalue=None):
            if (r := compare(a, b)) != Ord.EQ:
                return r
        return Ord.EQ

    # one of x or y is a list
    return compare(promote(x), promote(y))


def partition(xs: list[Any], pivot: Any) -> int:
    # O(n)
    i = xs.index(pivot)

    xs[-1], xs[i] = xs[i], xs[-1]

    idx = 0
    for i in range(0, len(xs) - 1):
        if compare(xs[i], pivot) in [Ord.LT, Ord.EQ]:  # xs[i] <= pivot
            xs[idx], xs[i] = xs[i], xs[idx]
            idx += 1

    xs[idx], xs[-1] = xs[-1], xs[idx]

    return idx


def main():
    with open(0) as fp:
        packets = [
            tuple(map(json.loads, pair.split("\n")))
            for pair in fp.read().strip().split("\n\n")
        ]

    print(
        "Part 1:",
        sum(i for i, r in enumerate(starmap(compare, packets), start=1) if r == Ord.LT),
    )

    d2, d6 = [[2]], [[6]]  # divider packets
    packets = [d2, d6, *chain.from_iterable(packets)]
    i2 = 1 + partition(packets, pivot=d2)
    i6 = 1 + partition(packets, pivot=d6)
    print("Part 2:", i2 * i6)


if __name__ == "__main__":
    main()
