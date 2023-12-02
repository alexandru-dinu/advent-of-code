from functools import reduce

DIRS = {">": +1, "v": +1j}


class Grid:
    def __init__(self, pos):
        self.pos = pos
        self.h = int(max(z.imag for z in pos)) + 1
        self.w = int(max(z.real for z in pos)) + 1

    def show(self):
        for y in range(self.h):
            for x in range(self.w):
                print(self.pos[complex(x, y)], end="")
            print()

    def iter_kind(self, kind):
        for z in self.pos:
            if self.pos[z] == kind:
                yield z

    def wrap(self, z):
        return complex(z.real % self.w, z.imag % self.h)


def step(grid):
    moved = False

    for kind, dz in DIRS.items():
        will_move = set()

        for z in grid.iter_kind(kind):
            if grid.pos.get((nz := grid.wrap(z + dz))) == ".":
                will_move.add((z, nz))

        for z, nz in will_move:
            grid.pos[nz] = grid.pos[z]
            grid.pos[z] = "."

        moved |= len(will_move) > 0

    return grid, moved


def iterate(grid):
    grid, moved = step(grid)
    if not moved:
        return
    yield grid
    yield from iterate(grid)


def main():
    with open(0) as fp:
        pos = {
            complex(x, y): c
            for y, line in enumerate(fp)
            for x, c in enumerate(line.strip())
        }

    print("Result:", reduce(lambda acc, _: acc + 1, iterate(Grid(pos)), 1))


if __name__ == "__main__":
    main()
