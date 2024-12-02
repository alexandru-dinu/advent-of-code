import re

import numpy as np

# (face, s) -> (new face, new s)
# 0123 -> URDL
wrap = {
    (0, 0): (5, 3),
    (0, 1): (1, 3),
    (0, 2): (2, 0),
    (0, 3): (4, 3),
    (1, 0): (5, 2),
    (1, 1): (3, 1),
    (1, 2): (2, 1),
    (1, 3): (0, 1),
    (2, 0): (0, 2),
    (2, 1): (1, 2),
    (2, 2): (3, 0),
    (2, 3): (4, 0),
    (3, 0): (2, 2),
    (3, 1): (1, 1),
    (3, 2): (5, 1),
    (3, 3): (4, 1),
    (4, 0): (2, 3),
    (4, 1): (3, 3),
    (4, 2): (5, 0),
    (4, 3): (0, 3),
    (5, 0): (4, 2),
    (5, 1): (3, 2),
    (5, 2): (1, 0),
    (5, 3): (0, 0),
}


def ori2sides(ori):
    # what s do I see w.r.t. my current ori: front, right, back, left?

    # facing right
    if ori == 1 + 0j:
        return 1, 2, 3, 0

    # facing down
    if ori == 0 - 1j:
        return 2, 3, 0, 1

    # facing left
    if ori == -1 + 0j:
        return 3, 0, 1, 2

    # facing up
    if ori == 0 + 1j:
        return 0, 1, 2, 3

    assert False


class Cube:
    SIDE = 50

    def __init__(self, faces):
        self.faces = faces

    def show_face(self, i):
        print("\n".join(map("".join, self.faces[i])))


class Ant:
    def __init__(self, cube):
        # start at the top-left corner of the 0 face, facing right
        self.cube = cube
        self.face = 0
        self.pos = 0 + 0j
        self.ori = 1 + 0j
        self.sides = ori2sides(self.ori)

    def at(self, pos):
        x, y = int(pos.real), int(pos.imag)
        return self.cube[self.face][y, x]

    def step(self):
        # move a single step forward
        peek = self.pos + self.ori
        x, y = int(peek.real), int(peek.imag)

        if (0 <= x < self.cube.SIDE) and (0 <= y < self.cube.SIDE):
            if self.at(peek) == "#":
                return  # can't move forward
            self.pos = peek
        else:
            # wrap around
            nf, ns = wrap[self.face, self.sides[0]]
            self.face = nf

    def rotate(self, ori):
        if ori == "L":
            self.ori *= 1j
        elif ori == "R":
            self.ori *= -1j
        else:
            assert False

        self.sides = ori2sides(self.ori)


def parse_cube(cube: str) -> Cube:
    """
    face parsing order:
        0 1
        2
      4 3
      5
    """

    s = Cube.SIDE

    lines = [x.strip() for x in cube.split("\n")]

    f01 = lines[:s]
    f0 = [list(x[:s]) for x in f01]
    f1 = [list(x[s:]) for x in f01]
    f2 = [list(x) for x in lines[s : 2 * s]]
    f43 = lines[2 * s : 3 * s]
    f4 = [list(x[:s]) for x in f43]
    f3 = [list(x[s:]) for x in f43]
    f5 = [list(x) for x in lines[3 * s :]]

    return Cube(list(map(np.array, [f0, f1, f2, f3, f4, f5])))


def move(cube, insts): ...


def main():
    with open(0) as fp:
        # cube is unfolded
        cube, insts = fp.read().split("\n\n")

    insts = [x if x in "LR" else int(x) for x in re.findall(r"(\d+|[LR])", insts)]

    cube = parse_cube(cube)

    cube.show_face(0)


if __name__ == "__main__":
    main()
