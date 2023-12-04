from dataclasses import dataclass
from typing import TextIO


class Grid:
    def move(self, instrs: str) -> "Grid":
        for c in instrs:
            match c:
                case "U":
                    z = self.z + 1j
                case "D":
                    z = self.z - 1j
                case "L":
                    z = self.z - 1
                case "R":
                    z = self.z + 1

            if self.can_move(z):
                self.z = z

        return self


@dataclass
class Grid1(Grid):
    pad = {
        -1 + 1j: "1",
        0 + 1j: "2",
        1 + 1j: "3",
        -1 + 0j: "4",
        0 + 0j: "5",
        1 + 0j: "6",
        -1 - 1j: "7",
        0 - 1j: "8",
        1 - 1j: "9",
    }
    z = 0 + 0j

    @staticmethod
    def can_move(z: complex) -> bool:
        return -1 <= z.real <= 1 and -1 <= z.imag <= 1


@dataclass
class Grid2(Grid):
    pad = {
        0 + 2j: "1",
        -1 + 1j: "2",
        0 + 1j: "3",
        1 + 1j: "4",
        -2 + 0j: "5",
        -1 + 0j: "6",
        0 + 0j: "7",
        1 + 0j: "8",
        2 + 0j: "9",
        -1 - 1j: "A",
        0 - 1j: "B",
        1 - 1j: "C",
        0 - 2j: "D",
    }
    z = -2 + 0j

    @staticmethod
    def can_move(z: complex) -> bool:
        return abs(z.real) + abs(z.imag) <= 2


def get_code(grid: Grid, lines: list[str]) -> str:
    c = ""

    for line in lines:
        grid.move(line)
        c += grid.pad[grid.z]

    return c


def solve(fp: TextIO) -> tuple[str, str]:
    lines = [line.strip() for line in fp]

    p1 = get_code(Grid1(), lines)
    p2 = get_code(Grid2(), lines)

    return p1, p2


def test_example() -> None:
    with open("example") as fp:
        p1, p2 = solve(fp)

    assert p1 == "1985"
    assert p2 == "5DB3"


def main() -> None:
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
