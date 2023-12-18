from itertools import pairwise
from pathlib import Path
from typing import TextIO


def get_inp(fp):
    DIRS = {"R": 1, "L": -1, "U": -1j, "D": 1j}
    instr = []

    for line in fp:
        d, n, c = line.strip().split()
        instr.append((DIRS[d], int(n), c[2:-1]))

    return instr


def area(xss: list[tuple]):
    # xss: [(dir, dist)]

    z = 0 + 0j
    ext = [z]
    ext_len = 0

    for d, n in xss:
        z += d * n
        ext_len += n
        ext.append(z)

    # Shoelace + Pick's theorem
    shoelace = sum(
        (a.real * b.imag) - (a.imag * b.real) for a, b in pairwise([*ext, ext[0]])
    )
    assert shoelace > 0
    inside = 1 + (shoelace - ext_len) // 2

    return int(inside + ext_len)


def solve(fp: TextIO):
    instr = get_inp(fp)

    p1 = area([(d, n) for d, n, _ in instr])
    p2 = area([([1, 1j, -1, -1j][int(c[-1])], int(c[:5], 16)) for _, _, c in instr])

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 62
    assert p2 == 952408144115


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
