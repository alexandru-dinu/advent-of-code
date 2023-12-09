import itertools as it
import random
import sys

from rich import print
from tqdm import trange


def run(prog: list[str], inp: str) -> bool:
    assert sum(1 for i in prog if i.startswith("inp")) == len(inp)

    vs = {v: 0 for v in "wxyz"}
    inp = iter(inp)

    for inst in prog:
        try:
            cmd, a, b = inst.split()
        except ValueError:
            cmd, a = inst.split()

        match cmd:
            case "inp":
                vs[a] = int(next(inp))
            case "add":
                vs[a] += vs[b] if b in vs else int(b)
            case "mul":
                vs[a] *= vs[b] if b in vs else int(b)
            case "div":
                vs[a] //= vs[b] if b in vs else int(b)
            case "mod":
                vs[a] %= vs[b] if b in vs else int(b)
            case "eql":
                vs[a] = int(vs[a] == (vs[b] if b in vs else int(b)))
            case _:
                raise ValueError(f"Unknown {cmd=}")

    return vs


def diff(p1, p2):
    assert len(p1) == len(p2)
    return [i for i in range(len(p1)) if p1[i] != p2[i]]


def rev(w, i1, i2, i3, z):
    assert z >= 0
    x = z % 26 + i1
    z //= i2
    if x != w:
        z *= 26
        z += w + i3
    return z


def run_rev(w):
    z = 0
    for _w, i1, i2, i3 in zip(w, I1, I2, I3):
        z = rev(int(_w), i1, i2, i3, z)
        print(_w, z)
    return z


def test_rev(prog, niter):
    for _ in trange(niter):
        w = "".join(str(random.randint(1, 9)) for _ in range(14))
        assert run_rev(w) == run(prog, w)["z"]


def search(space, pushpop):
    w = [0] * 14
    assert len(pushpop) == 7
    for p in it.product(space, repeat=len(pushpop)):
        for i, (push, pop) in enumerate(pushpop):
            w[push] = p[i]
            w[pop] = I1[pop] + w[push] + I3[push]
            if not 0 < w[pop] < 10:
                break
        else:
            break

    return "".join(map(str, w))


I1 = []
I2 = []
I3 = []


def populate_Is(subprogs):
    global I1, I2, I3
    for sp in subprogs:
        I2.append(int(sp[3].split()[-1]))
        I1.append(int(sp[4].split()[-1]))
        I3.append(int(sp[14].split()[-1]))


def find_push_pop(xs):
    s = []
    ret = []

    for i, x in enumerate(xs):
        if x == 1:
            s.append(i)
        elif x == 26:
            j = s.pop()
            ret.append((j, i))
        else:
            assert False

    assert len(s) == 0
    return sorted(ret, key=lambda xy: xy[0])


def main():
    with open(sys.argv[1]) as fp:
        prog = [l.strip() for l in fp.readlines()]

    subprogs = []
    for k, g in it.groupby(prog, key=lambda x: x == "inp w"):
        if not k:  # not matching 'inp w'
            subprogs.append(list(g))
    assert len(subprogs) == 14
    assert all(len(sp) == 17 for sp in subprogs)

    populate_Is(subprogs)
    pushpop = find_push_pop(I2)

    if 0:
        test_rev(prog, 500_000)
    else:
        space = list(range(1, 10))

        w = search(space[::-1], pushpop)
        assert run(prog, w)["z"] == 0
        print(f"Part 1: {w}")

        w = search(space, pushpop)
        assert run(prog, w)["z"] == 0
        print(f"Part 2: {w}")

    exit(0)
    ################################
    ## Exploration

    # the only diff. instructions are @ 3, 4, 14
    for p1, p2 in it.combinations(subprogs, 2):
        assert set(diff(p1, p2)) <= {3, 4, 14}

    # instr. involving w are @ 5, 13, so they're the same for all subprogs:
    # eql x w
    # add y w
    assert all(
        {5, 13} == {i for i, inst in enumerate(sp) if "w" in inst} for sp in subprogs
    )

    # special = {k: defaultdict(list) for k in [3, 4, 14]}
    # for j, sp in enumerate(subprogs):
    #     for i in [3, 4, 14]:
    #         special[i][sp[i]].append(j)
    # print(special)

    for sp in subprogs:
        print(sp[4], "|", sp[3], "|", sp[14])

    # import os

    # while True:
    #     a = int(input())
    #     b = int(input())

    #     os.system("clear")
    #     for i, (p1, p2) in enumerate(zip(subprogs[a], subprogs[b])):
    #         s = f"{i:2d}: {p1:12s} -- {p2:12s}"
    #         if p1 != p2:
    #             s = f"[red]{s}"
    #         if "w" in p1 or "w" in p2:
    #             s = f"[italic]{s}"
    #         print(s)


if __name__ == "__main__":
    main()
