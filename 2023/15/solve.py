from dataclasses import dataclass
from functools import reduce
from pathlib import Path
from typing import TextIO


def hashstr(xs: str):
    return reduce(lambda z, x: (17 * (z + ord(x))) % 256, xs, 0)


@dataclass
class Lens:
    lbl: str
    num: int

    def __eq__(self, other: str):
        return self.lbl == other


def score(boxes):
    return sum(
        (i + 1) * (j + 1) * l.num for i, ls in boxes.items() for j, l in enumerate(ls)
    )


def solve(fp: TextIO):
    xss = fp.read().strip().split(",")

    boxes = {h: [] for h in range(256)}

    for xs in xss:
        if "=" in xs:
            lbl, n = xs.split("=")
            h = hashstr(lbl)
            l = Lens(lbl, int(n))

            if l in boxes[h]:
                boxes[h][boxes[h].index(l)] = l  # overwrite with new num
            else:
                boxes[h].append(l)

        elif "-" in xs:
            lbl = xs[:-1]
            try:
                boxes[hashstr(lbl)].remove(lbl)
            except ValueError:
                pass

    p1 = sum(map(hashstr, xss))
    p2 = score(boxes)

    return p1, p2


def test_example():
    assert hashstr("HASH") == 52

    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 1320
    assert p2 == 145


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
