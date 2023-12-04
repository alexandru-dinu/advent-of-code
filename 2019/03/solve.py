from dataclasses import dataclass


@dataclass
class Point:
    x: int
    y: int

    def __add__(self, other):
        return Point(self.x + other.x, self.y + other.y)


@dataclass
class Segment:
    p1: Point
    p2: Point

    def intersect(self, other: "Segment"):
        assert False


def parse_seg(dirs: list[str]) -> list[Point]:
    ps = [Point(0, 0)]

    for d in dirs:
        l = int(d[1:])
        match d[0]:
            case "U":
                p = ps[-1] + Point(0, l)
            case "D":
                p = ps[-1] + Point(0, -l)
            case "L":
                p = ps[-1] + Point(-l, 0)
            case "R":
                p = ps[-1] + Point(l, 0)
        ps.append(p)

    return ps


def main():
    with open(0) as fp:
        s1, s2 = map(lambda l: parse_seg(l.strip().split(",")), fp.readlines())

    print(s1)


if __name__ == "__main__":
    main()
