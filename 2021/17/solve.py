from __future__ import annotations

import re
import sys
from dataclasses import dataclass
from math import sqrt


@dataclass
class Point:
    x: float
    y: float
    dx: float
    dy: float

    def step(self) -> Point:
        self.x += self.dx
        self.y += self.dy
        self.dx -= sign(self.dx)
        self.dy -= 1

        return self


def sign(x):
    return (x > 0) - (x < 0)


def parse_input(file: str) -> tuple[int]:
    n_re = r"(-?\d+)"
    in_re = re.compile(f"target area: x={n_re}..{n_re}, y={n_re}..{n_re}")

    with open(file) as fp:
        xl, xh, yl, yh = map(int, re.findall(r"-?\d+", fp.read()))

    # we'll make the assumption that the target area is
    # below and to the right of the starting position
    assert yl < yh < 0
    assert 0 < xl < xh

    return xl, xh, yl, yh


def tri(n: float) -> float:
    return 0.5 * n * (n + 1)


def part1(xl, xh, yl, yh) -> float:
    """Return (dx, dy, H)"""
    # tri(a) = x => a^2 + a - 2x = 0 => a = 0.5 * (-1+sqrt(1+8x))
    a = None
    for x in range(xl, xh + 1):
        a = 0.5 * (-1 + sqrt(1 + 8 * x))
        if a.is_integer():
            a = int(a)
            break
    else:
        assert False, "Can't satisfy tri(a) = x."

    # see README
    b = -yl - 1

    return int(tri(b))


def part2(xl, xh, yl, yh):
    overshoot = lambda p: (p.x > xh) or (p.y < yl)
    inside = lambda p: (xl <= p.x <= xh) and (yl <= p.y <= yh)

    count = 0

    for dx in range(1, xh + 1):
        for dy in range(yl, -yl):
            p = Point(x=0, y=0, dx=dx, dy=dy)

            while True:
                if overshoot(p):
                    break
                if inside(p):
                    count += 1
                    break
                p = p.step()

    return count


def main():
    xl, xh, yl, yh = parse_input(sys.argv[1])

    print("Part 1:", part1(xl, xh, yl, yh))
    print("Part 2:", part2(xl, xh, yl, yh))


if __name__ == "__main__":
    main()
