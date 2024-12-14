import re
from collections import Counter
from copy import deepcopy
from pathlib import Path
from typing import TextIO

import numpy as np
import pandas as pd
from tqdm import trange


def step(px, py, vx, vy, w, h, num):
    for i in range(num):
        px = (px + vx) % w
        py = (py + vy) % h

    return px, py


def count_quadrant(cnt, sx, ex, sy, ey):
    tot = 0
    for (px, py), m in cnt.items():
        if sx <= px < ex and sy <= py < ey:
            tot += m
    return tot


def parse(fp: TextIO):
    pos, vel = [], []
    for line in fp.readlines():
        px, py, vx, vy = map(int, re.match(r"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)", line).groups())
        pos.append((px, py))
        vel.append((vx, vy))

    return pos, vel


def solve1(pos, vel, w, h):
    cnt = Counter([step(*p, *v, w, h, num=100) for p, v in zip(pos, vel)])
    qs = [
        count_quadrant(cnt, 0, w // 2, 0, h // 2),
        count_quadrant(cnt, w // 2 + 1, w, 0, h // 2),
        count_quadrant(cnt, 0, w // 2, h // 2 + 1, h),
        count_quadrant(cnt, w // 2 + 1, w, h // 2 + 1, h),
    ]

    return np.prod(qs)


def P(mat):
    w = len(mat[0])
    h = len(mat)
    print("\n".join("".join(mat[i][j] for j in range(w)) for i in range(h)))


def solve2(pos, vel, w, h):
    deviations = []
    hist = [deepcopy(pos)]

    # simulate
    for level in trange(1, 10_000 + 1):
        for i in range(len(pos)):
            pos[i] = step(*pos[i], *vel[i], w, h, num=1)
        hist.append(deepcopy(pos))

        dx, dy = zip(*pos)
        vx = np.std(dx)
        vy = np.std(dy)
        deviations.append({"level": level, "vx": vx, "vy": vy})

    # find the level with the smallest x&y deviations; will contain the tree
    deviations = pd.DataFrame(deviations)
    res = deviations.sort_values(["vx", "vy", "level"]).iloc[0].to_dict()
    level = int(res["level"])

    mat = [list(" " * w) for _ in range(h)]
    for x, y in hist[level]:
        mat[y][x] = "X"
    P(mat)
    print(res)

    return level


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        pos, vel = parse(fp)
        assert 12 == solve1(pos, vel, w=11, h=7)


def main():
    with open(Path(__file__).parent / "input") as fp:
        pos, vel = parse(fp)

    p1 = solve1(pos, vel, w=101, h=103)
    p2 = solve2(pos, vel, w=101, h=103)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
