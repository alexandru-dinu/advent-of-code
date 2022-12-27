# Upsolving: instead of using an array, we can just store the occupied positions in a set.

from collections import defaultdict
from itertools import count

# NW, N, NE, W, E, SW, S, SE
OFFSETS = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]


def nei(z: complex) -> set[complex]:
    return {z + complex(*o) for o in OFFSETS}


def step_round(grid, k):
    moved = False
    proposals = defaultdict(list)

    # 1/ collect proposals
    for elf in grid:
        # if no other occupied pos, the elf does not move
        if not nei(elf) & grid:
            continue

        deltas = (
            # N, NE, NW
            (elf - 1j, not any((elf - 1j + o) in grid for o in (-1, 0, 1))),
            # S, SE, SW
            (elf + 1j, not any((elf + 1j + o) in grid for o in (-1, 0, 1))),
            # W, NW, SW
            (elf - 1, not any((elf - 1 + o * 1j) in grid for o in (-1, 0, 1))),
            # E, NE, SE
            (elf + 1, not any((elf + 1 + o * 1j) in grid for o in (-1, 0, 1))),
        )
        for q in range(4):
            nxt, cond = deltas[(k + q) % 4]
            if cond:
                proposals[nxt].append(elf)
                break

    # 2/ apply proposals
    # the elf moves to its proposal if and only if it is the only elf who proposed it
    for nxt, elf_pos in proposals.items():
        if len(elf_pos) == 1:
            grid.add(nxt)
            grid.remove(elf_pos.pop())
            moved = True

    return grid, moved


def get_bb(grid):
    # get bounding box containing all elves
    min_x = int(min(x.real for x in grid))
    max_x = int(max(x.real for x in grid))
    min_y = int(min(x.imag for x in grid))
    max_y = int(max(x.imag for x in grid))

    return min_x, max_x, min_y, max_y


def show_grid(grid):
    min_x, max_x, min_y, max_y = get_bb(grid)
    for y in range(min_y, max_y + 1):
        for x in range(min_x, max_x + 1):
            print("#" if complex(x, y) in grid else ".", end="")
        print()


def part1(grid):
    for k in range(10):
        grid, _ = step_round(grid, k % 4)

    min_x, max_x, min_y, max_y = get_bb(grid)
    return (max_x - min_x + 1) * (max_y - min_y + 1) - len(grid)


def part2(grid):
    for k in count(0):
        grid, moved = step_round(grid, k % 4)
        if not moved:
            return k + 1


def main():
    with open(0) as fp:
        grid = {
            complex(x, y)
            for y, line in enumerate(fp)
            for x, c in enumerate(line.strip())
            if c == "#"
        }

    print("Part 1:", part1(grid.copy()))
    print("Part 2:", part2(grid.copy()))


if __name__ == "__main__":
    main()
