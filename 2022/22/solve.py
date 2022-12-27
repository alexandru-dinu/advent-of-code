"""
Idea from
https://www.reddit.com/r/adventofcode/comments/zsct8w/comment/j17k7nn
enclose cube in a 4(y)x3(x) grid:
  0 1 2
0 . # #
1 . # .
2 # # .
3 # . .
"""

import re

SIDE = 50


def parse_cube(lines) -> dict[complex, str]:
    cube = {}

    for y, line in enumerate(lines.split("\n")):
        for x, c in enumerate(line):
            if c in ".#":
                cube[complex(x, y)] = c

    return cube


def get_pwd(pos, ori):
    x, y = pos.real, pos.imag
    return int(
        sum(
            (
                1000 * (y + 1),
                4 * (x + 1),
                [1, 1j, -1, -1j].index(ori),
            )
        )
    )


def part1(cube, insts):
    # start
    pos = complex(min(z.real for z in cube if z.imag == 0 and cube[z] == "."), 0)
    ori = 1 + 0j
    # our y-axis is flipped (down is positive)
    # need to account for this when rotating

    for d in insts:
        if d == "L":
            ori *= -1j
        elif d == "R":
            ori *= 1j
        else:
            for _ in range(d):
                nxt = pos + ori

                if nxt in cube:
                    if cube[nxt] == "#":
                        break
                    else:
                        pos = nxt
                        continue

                # we're off the grid, wrap
                # move back until the first walkable cell in the grid
                nxt -= ori
                before_fall = nxt
                while nxt in cube:
                    nxt -= ori
                nxt += ori

                # now I'm back on the grid
                # if I'm standing on a walkable cell, update pos
                if cube[nxt] == ".":
                    pos = nxt
                    continue

                assert cube[nxt] == "#"
                pos = before_fall

    return pos, ori


def wrap_3d(ori, pos):
    # new (ori, pos) after wrapping
    # there are 14 total cases when we have to wrap (exterior sides)
    x, y = pos.real, pos.imag
    ix, iy = x // SIDE, y // SIDE

    # right
    if ori == 1:
        if iy == 0:
            return -1, complex(2 * SIDE - 1, 3 * SIDE - 1 - y)
        if iy == 1:
            return -1j, complex(2 * SIDE + y - SIDE, SIDE - 1)
        if iy == 2:
            return -1, complex(3 * SIDE - 1, 3 * SIDE - 1 - y)
        if iy == 3:
            return -1j, complex(SIDE + y - 3 * SIDE, 3 * SIDE - 1)

    # down
    if ori == 1j:
        if ix == 0:
            return 1j, complex(2 * SIDE + x, 0)
        if ix == 1:
            return -1, complex(SIDE - 1, 3 * SIDE + x - SIDE)
        if ix == 2:
            return -1, complex(2 * SIDE - 1, SIDE + x - 2 * SIDE)

    # left
    if ori == -1:
        if iy == 0:
            return 1, complex(0, 3 * SIDE - y - 1)
        if iy == 1:
            return 1j, complex(y - SIDE, 2 * SIDE)
        if iy == 2:
            return 1, complex(SIDE, 3 * SIDE - y - 1)
        if iy == 3:
            return 1j, complex(SIDE + y - 3 * SIDE, 0)

    # up
    if ori == -1j:
        if ix == 0:
            return 1, complex(SIDE, SIDE + x)
        if ix == 1:
            return 1, complex(0, 3 * SIDE + x - SIDE)
        if ix == 2:
            return -1j, complex(x - 2 * SIDE, 4 * SIDE - 1)

    # we covered all cases, so this should never happen
    assert False, (ori, pos)


def part2(cube, insts):
    # start
    pos = complex(min(z.real for z in cube if z.imag == 0 and cube[z] == "."), 0)
    ori = 1 + 0j
    # our y-axis is flipped (down is positive)
    # need to account for this when rotating

    for d in insts:
        # print(
        #     "@",
        #     pos.imag + 1,
        #     pos.real + 1,
        #     "RDLU"[[1, 1j, -1, -1j].index(ori)],
        #     "got",
        #     d,
        # )
        if d == "L":
            ori *= -1j
        elif d == "R":
            ori *= 1j
        else:
            for _ in range(d):
                nxt = pos + ori

                if nxt in cube:
                    if cube[nxt] == "#":
                        break
                    else:
                        pos = nxt
                        continue

                # we're off the grid, wrap
                nori, nxt = wrap_3d(ori, nxt)

                if cube[nxt] == "#":
                    break

                assert cube[nxt] == "."
                pos = nxt
                ori = nori
                # print(
                #     "\twrapping to ",
                #     pos.imag + 1,
                #     pos.real + 1,
                #     "RDLU"[[1, 1j, -1, -1j].index(ori)],
                # )

    return pos, ori


def main():
    with open(0) as fp:
        # cube is unfolded
        cube_lines, insts = fp.read().split("\n\n")
        insts = [x if x in "LR" else int(x) for x in re.findall(r"(\d+|[LR])", insts)]
        cube = parse_cube(cube_lines)

    pos, ori = part1(cube, insts)
    print("Part 1:", get_pwd(pos, ori))

    pos, ori = part2(cube, insts)
    print("Part 2:", get_pwd(pos, ori))


if __name__ == "__main__":
    main()
