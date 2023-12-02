from dataclasses import dataclass


@dataclass
class Point:
    x: int
    y: int


@dataclass
class Segment:
    start: Point
    end: Point

    def crosses(self, other):
        for s1, s2 in [(self, other), (other, self)]:
            x1, x2 = sorted([s1.start.x, s1.end.x])
            c1 = x1 < s2.start.x < x2

            y1, y2 = sorted([s2.start.y, s2.end.y])
            c2 = y1 < s1.start.y < y2

            if c1 and c2:
                return Point(s2.start.x, s1.start.y)

        return None


def part1(dirs):
    dx, dy = 0, 0
    ori = 0 + 1j  # start facing north

    for o, n in dirs:
        if o == "R":
            ori *= -1j  # -90deg (CW)
        if o == "L":
            ori *= 1j  # +90deg (CCW)

        dx += n * ori.real
        dy += n * ori.imag

    return abs(dx) + abs(dy)


def part2(dirs):
    pos = Point(0, 0)
    ori = 0 + 1j  # start facing north
    seen = []

    for o, n in dirs:
        if o == "R":
            ori *= -1j  # -90deg (CW)
        if o == "L":
            ori *= 1j  # +90deg (CCW)

        prev = Point(pos.x, pos.y)
        pos.x += n * ori.real
        pos.y += n * ori.imag

        new = Segment(prev, Point(pos.x, pos.y))

        for other in seen:
            if (c := new.crosses(other)) is not None:
                return abs(c.x) + abs(c.y)

        seen.append(new)

    assert False, "unreachable"


def main():
    with open(0) as fp:
        dirs = [(x[0], int(x[1:])) for x in fp.read().split(", ")]

    print("Part 1:", int(part1(dirs)))
    print("Part 2:", int(part2(dirs)))


if __name__ == "__main__":
    main()
