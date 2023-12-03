import itertools as it
import re
from dataclasses import dataclass


@dataclass
class Num:
    row: int
    col: int
    end: int
    val: int
    reachable: bool = False

    def __hash__(self):
        return hash((self.row, self.col, self.val))


@dataclass
class Sym:
    row: int
    col: int
    val: str


def parse_input(fp):
    grid = {}
    syms = []
    nums = []

    for row, line in enumerate(fp):
        line = line.strip()

        for m in re.finditer(r"\d+", line):
            col, end = m.span()
            n = Num(row=row, col=col, end=end, val=int(m.group()))
            nums.append(n)
            for i in range(col, end):
                grid[row, i] = n

        for m in re.finditer(r"[^\d\.]", line):
            p = (row, m.start())
            s = Sym(row=row, col=m.start(), val=m.group())
            grid[p] = s
            syms.append(s)

    return grid, syms, nums


def nei(i, j) -> list[tuple]:
    return [
        (i + di, j + dj)
        for di, dj in it.product([-1, 0, 1], repeat=2)
        if (di, dj) != (0, 0)
    ]


def part1(grid, syms, nums) -> int:
    for sym in syms:
        for p in nei(sym.row, sym.col):
            if p in grid and isinstance(grid[p], Num):
                grid[p].reachable = True

    return sum(num.val for num in nums if num.reachable)


def part2(grid, syms, nums) -> int:
    res = 0

    for sym in filter(lambda s: s.val == "*", syms):
        adj = set()

        for p in nei(sym.row, sym.col):
            if p in grid and isinstance(grid[p], Num):
                adj.add(grid[p])

        if len(adj) != 2:
            continue

        res += adj.pop().val * adj.pop().val

    return res


def test_example():
    from pathlib import Path

    with open(Path(__file__).parent / "example") as fp:
        grid, syms, nums = parse_input(fp)

    assert part1(grid, syms, nums) == 4361
    assert part2(grid, syms, nums) == 467835


def main():
    with open(0) as fp:
        grid, syms, nums = parse_input(fp)

    print(f"Part 1: {part1(grid, syms, nums)}")
    print(f"Part 2: {part2(grid, syms, nums)}")


if __name__ == "__main__":
    main()
