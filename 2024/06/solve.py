from pathlib import Path
from typing import TextIO

from tqdm import tqdm


def walk(pos, ori, grid) -> tuple[set, bool]:
    path = {(pos, ori)}
    has_cycle = False

    while pos in grid:
        match grid.get(pos + ori):
            case None:  # outside
                break
            case "#":  # obstacle, turn right and move
                ori *= 1j
            case _:
                pos += ori

        # cycle
        if (pos, ori) in path:
            has_cycle = True
            break

        path.add((pos, ori))

    return {p for p, _ in path}, has_cycle


def count_loops(pos, ori, grid, candidates):
    results = []

    tot = 0
    for new in tqdm(candidates):
        _, has_cycle = walk(pos, ori, grid | {new: "#"})
        tot += has_cycle

    return tot


def solve(fp: TextIO):
    grid = {
        complex(j, i): x
        for i, row in enumerate(fp.read().strip().split("\n"))
        for j, x in enumerate(row)
    }
    pos = min(p for p in grid if grid[p] == "^")
    ori = -1j

    path, _ = walk(pos, ori, grid)
    p1 = len(path)
    p2 = count_loops(pos, ori, grid, candidates=path)

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 41
    assert p2 == 6


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
