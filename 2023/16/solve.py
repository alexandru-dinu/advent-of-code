import multiprocessing as mp
from dataclasses import dataclass
from functools import partial
from pathlib import Path
from typing import TextIO

import numpy as np


def get_inp(fp: TextIO):
    return np.array([list(x.strip()) for x in fp])


@dataclass
class Beam:
    pos: complex
    ori: complex

    def __str__(self):
        return f"({self.pos})[{self.ori}]"

    def __hash__(self):
        return hash((self.pos, self.ori))

    def to_rc(self):
        return int(self.pos.imag), int(self.pos.real)

    @staticmethod
    def from_rco(r, c, o):
        return Beam(pos=(c + r * 1j), ori=o)

    def is_valid_in(self, g):
        h, w = g.shape
        y, x = self.to_rc()

        return (0 <= y < h) and (0 <= x < w)

    # TODO: can be simplified
    def advance_in(self, g: np.ndarray) -> list["Beam"]:
        assert self.is_valid_in(g)
        y, x = self.to_rc()

        match g[y, x]:
            case ".":
                return [Beam(pos=self.pos + self.ori, ori=self.ori)]
            case "|":
                match self.ori:
                    case 1j | -1j:
                        return [Beam(pos=self.pos + self.ori, ori=self.ori)]
                    case 1 | -1:
                        return [
                            Beam.from_rco(y - 1, x, -1j),
                            Beam.from_rco(y + 1, x, 1j),
                        ]
            case "-":
                match self.ori:
                    case 1 | -1:
                        return [Beam(pos=self.pos + self.ori, ori=self.ori)]
                    case 1j | -1j:
                        return [Beam.from_rco(y, x - 1, -1), Beam.from_rco(y, x + 1, 1)]
            case "/":
                return [
                    {
                        -1j: Beam.from_rco(y, x + 1, 1),
                        1j: Beam.from_rco(y, x - 1, -1),
                        1: Beam.from_rco(y - 1, x, -1j),
                        -1: Beam.from_rco(y + 1, x, 1j),
                    }[self.ori]
                ]
            case "\\":
                return [
                    {
                        -1j: Beam.from_rco(y, x - 1, -1),
                        1j: Beam.from_rco(y, x + 1, 1),
                        1: Beam.from_rco(y + 1, x, 1j),
                        -1: Beam.from_rco(y - 1, x, -1j),
                    }[self.ori]
                ]

        assert False


def show(g):
    print("\n".join("".join(r) for r in g))


def count_energized(start, grid):
    beams = [start]
    vis = set()

    while beams:
        b = beams.pop()

        # beam is id'd by pos & ori
        if b in vis:
            continue

        vis.add(b)
        beams.extend([b2 for b2 in b.advance_in(grid) if b2.is_valid_in(grid)])

    # just count the visited positions, regardless or orientation
    return len({b.pos for b in vis})


def solve(fp: TextIO):
    g = get_inp(fp)

    p1 = count_energized(start=Beam.from_rco(0, 0, 1), grid=g)

    h, w = g.shape
    bs = []

    for x in range(w):
        bs.append(Beam.from_rco(0, x, 1j))
        bs.append(Beam.from_rco(h - 1, x, -1j))

    for y in range(h):
        bs.append(Beam.from_rco(y, 0, 1))
        bs.append(Beam.from_rco(y, w - 1, -1))

    with mp.Pool(mp.cpu_count()) as pool:
        p2 = max(pool.imap_unordered(partial(count_energized, grid=g), bs))

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 46
    assert p2 == 51


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
