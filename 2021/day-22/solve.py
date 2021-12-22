from __future__ import annotations

import re
import sys
from typing import *

import numpy as np

Instructions = List[Tuple[str, List[int]]]


def part1(inst: Instructions) -> int:
    f = filter(lambda x: all(abs(c) <= 50 for c in x[1]), inst)

    grid = np.zeros((101,) * 3, dtype=bool)

    for state, (x1, x2, y1, y2, z1, z2) in f:
        z = np.arange(z1, z2 + 1)
        y = np.arange(y1, y2 + 1)
        x = np.arange(x1, x2 + 1)

        grid[np.ix_(z, y, x)] = state == "on"

    return grid.sum()


def part2(inst: Instructions) -> int:
    raise NotImplementedError("TODO")


def main():
    r = r"(-?\d+)..(-?\d+)"
    r = re.compile(f"(on|off) x={r},y={r},z={r}")

    with open(sys.argv[1]) as fp:
        inst = fp.read().strip().split("\n")
        inst = [r.match(x).groups() for x in inst]
        inst = [(z, list(map(int, zs))) for (z, *zs) in inst]

    print("Part 1:", part1(inst))
    print("Part 2:", part2(inst))


if __name__ == "__main__":
    main()
