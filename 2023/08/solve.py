import math
import re
from itertools import cycle
from typing import TextIO


def get_inp(fp: TextIO):
    instr, dirs = fp.read().strip().split("\n\n")
    instr = [int(x) for x in instr.replace("L", "0").replace("R", "1")]

    d = {}
    for line in dirs.split("\n"):
        src, l, r = re.findall(r"[A-Z]{3}", line)
        d[src] = (l, r)

    return instr, d


def traverse(instr, dirs, start) -> list[str]:
    z = start
    path = []

    for steps, i in enumerate(cycle(instr), start=1):
        z = dirs[z][i]
        path.append(z)
        if z.endswith("Z"):
            return path


def solve(fp: TextIO):
    instr, dirs = get_inp(fp)

    p1 = len(traverse(instr, dirs, start="AAA"))

    """
    DPA --> NGZ (20778)
    QLA --> XMZ (19200)
    VJA --> GLZ (18674)
    GTA --> FXZ (16044)
    AAA --> ZZZ (12362)
    XQA --> HHZ (15518)

    obs: each node ending in Z will loop back to itself after the same number of steps
    """
    start = [k for k in dirs if k.endswith("A")]

    # asserting the observation
    for src in start:
        path = traverse(instr, dirs, src)
        dst = path[-1]
        n = len(path)

        path = traverse(instr, dirs, dst)
        assert dst == path[-1]
        assert len(path) == n

    lens = [len(traverse(instr, dirs, s)) for s in start]
    p2 = math.lcm(*lens)

    return p1, p2


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
