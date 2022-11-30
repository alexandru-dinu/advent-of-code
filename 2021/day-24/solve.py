from __future__ import annotations

import sys
from itertools import product


def run(prog: list[str], inp: str) -> bool:
    assert sum(1 for i in prog if i.startswith("inp")) == len(inp)

    vs = {v: 0 for v in "wxyz"}
    inp = iter(inp)

    for inst in prog:
        try:
            cmd, a, b = inst.split()
        except ValueError:
            cmd, a = inst.split()

        if cmd == "inp":
            vs[a] = int(next(inp))
        elif cmd == "add":
            vs[a] += vs[b] if b in vs else int(b)
        elif cmd == "mul":
            vs[a] *= vs[b] if b in vs else int(b)
        elif cmd == "div":
            vs[a] //= vs[b] if b in vs else int(b)
        elif cmd == "mod":
            vs[a] %= vs.get(b, int(b))
        elif cmd == "eql":
            lhs = vs[a] if a in vs else int(a)
            rhs = vs[b] if b in vs else int(b)
            vs[a] = int(lhs == rhs)
        else:
            raise ValueError(f"Unknown {cmd=}")

    return vs.get("z") == 0


def part1(prog: list[str]) -> str:
    for inp in product("987654321", repeat=14):
        inp = "".join(inp)
        if run(prog, inp):
            return inp

    assert False, "unreachable"


def main():
    with open(sys.argv[1]) as fp:
        prog = [l.strip() for l in fp.readlines()]

    print("Part 1:", part1(prog))


if __name__ == "__main__":
    main()
