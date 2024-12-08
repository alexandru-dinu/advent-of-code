from itertools import combinations
from pathlib import Path
from typing import TextIO


def part1(antennas_pos, grid):
    tot = set()

    for p1, p2 in combinations(antennas_pos, 2):
        if grid[p1] == grid[p2] and abs(p1 - p2) ** 2 >= 2:
            dx = p1.real - p2.real
            dy = p1.imag - p2.imag
            if (n := complex(p1.real + dx, p1.imag + dy)) in grid:
                tot.add(n)
            if (n := complex(p2.real - dx, p2.imag - dy)) in grid:
                tot.add(n)

    return len(tot)


def part2(antennas_pos, grid):
    tot = set()

    for p1, p2 in combinations(antennas_pos, 2):
        if grid[p1] == grid[p2]:
            dx = p1.real - p2.real
            dy = p1.imag - p2.imag

            tot.add(p1)
            tot.add(p2)

            k = 1
            while (n := complex(p1.real + k * dx, p1.imag + k * dy)) in grid:
                tot.add(n)
                k += 1

            k = 1
            while (n := complex(p2.real - k * dx, p2.imag - k * dy)) in grid:
                tot.add(n)
                k += 1

    return len(tot)


def solve(fp: TextIO):
    grid = {
        complex(j, i): x
        for i, row in enumerate(fp.read().strip().split("\n"))
        for j, x in enumerate(row)
    }

    antennas_pos = [k for k, v in grid.items() if v != "."]

    p1 = part1(antennas_pos, grid)
    p2 = part2(antennas_pos, grid)

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 14
    assert p2 == 34


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
