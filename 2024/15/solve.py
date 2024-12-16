from copy import deepcopy
from pathlib import Path
from typing import TextIO


def parse(fp: TextIO):
    grid, instr = fp.read().strip().split("\n\n")

    instr = "".join(instr.split("\n"))
    instr = [{"^": -1, "v": 1, "<": -1j, ">": 1j}[i] for i in instr]

    grid = {complex(i, j): c for i, row in enumerate(grid.split("\n")) for j, c in enumerate(row)}

    return grid, instr


def debug(grid, pos):
    w = 1 + int(max(p.imag for p in grid))
    h = 1 + int(max(p.real for p in grid))

    out = []
    for i in range(h):
        row = ""
        for j in range(w):
            z = complex(i, j)
            if z == pos:
                row += "@"
            else:
                row += grid[z]

        out.append(row)

    print("\n".join(out))


def solve1(grid, instr):
    cur = min(k for k, c in grid.items() if c == "@")
    grid[cur] = "."  # no need to save the actual robot on the grid

    for step, ori in enumerate(instr, start=1):
        nxt = cur + ori
        if nxt not in grid or grid[nxt] == "#":
            continue
        if grid[nxt] == ".":
            cur = nxt
        else:
            # try to move on a box, should push it/them as far as possible IF possible
            assert grid[nxt] == "O"

            # find where the run of boxes terminates
            j = nxt
            while grid[j] == "O":
                j += ori

            # can't move b/c all the boxes lead to a wall
            if grid[j] == "#":
                continue

            # move boxes simply by swapping the first one w/ the empty space found
            assert grid[j] == "."
            grid[j], grid[nxt] = grid[nxt], grid[j]
            cur = nxt

        # debug(grid, cur)
        # input(f'[{step=}] >>>')

    return int(sum(100 * z.real + z.imag for z, x in grid.items() if x == "O"))


def solve2(grid, instr):
    # enlarge the grid
    grid2 = {}
    for z, x in grid.items():
        z = complex(z.real, 2 * z.imag)
        match x:
            case "." | "#":
                grid2[z] = x
                grid2[z + 1j] = x
            case "O":
                grid2[z] = "["
                grid2[z + 1j] = "]"
            case "@":
                grid2[z] = x
                grid2[z + 1j] = "."
            case _:
                assert False  # should not happen
    assert len(grid2) == 2 * len(grid)
    grid = grid2

    cur = min(k for k, c in grid.items() if c == "@")
    grid[cur] = "."  # no need to save the actual robot on the grid
    debug(grid, cur)

    # TODO: shall we work on the original grid (not twice as wide?)

    for step, ori in enumerate(instr, start=1):
        nxt = cur + ori
        if nxt not in grid or grid[nxt] == "#":
            continue
        if grid[nxt] == ".":
            cur = nxt
        else:
            print('???', nxt)
            # try to move on a box, should push it/them as far as possible IF possible
            assert grid[nxt] in "[]"

            # find where the run of boxes terminates
            j = nxt
            while grid[j] in "[]":
                j += ori

            # can't move b/c all the boxes lead to a wall
            if grid[j] == "#":
                continue

            assert grid[j] == "."
            for k in range(int(abs(j - nxt)), -1, -1):
                grid[nxt+k*ori] = grid[nxt + (k-1) * ori]
            grid[cur] = '.'
            cur = nxt

        debug(grid, cur)
        input(f'[{step=}][{ori=}] >>>')

    return int(sum(100 * z.real + z.imag for z, x in grid.items() if x == "["))


def _test_example():
    with open(Path(__file__).parent / "example") as fp:
        grid, instr = parse(fp)

    p1 = solve1(deepcopy(grid), instr)
    p2 = solve2(deepcopy(grid), instr)

    assert p1 == 10092
    assert p2 == 9021


def main():
    with open(Path(__file__).parent / "example2") as fp:
        grid, instr = parse(fp)

    p1 = solve1(deepcopy(grid), instr)
    p2 = solve2(deepcopy(grid), instr)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
