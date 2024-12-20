from collections import defaultdict
from typing import TextIO

from tqdm import trange


def dfs(grid, S, E):
    path = [S]
    vis = {S}

    while path[-1] != E:
        cur = path[-1]

        for dz in [-1, 1, -1j, 1j]:
            nxt = cur + dz
            if nxt in vis or nxt not in grid or grid[nxt] == "#":
                continue
            path.append(nxt)
            vis.add(nxt)
            break

    return path


def manh(z1, z2):
    return abs(z1.real - z2.real) + abs(z1.imag - z2.imag)


def solve(fp: TextIO):
    grid = {
        complex(i, j): x
        for i, row in enumerate(fp.read().strip().split("\n"))
        for j, x in enumerate(row)
    }
    S = min(k for k in grid if grid[k] == "S")
    E = min(k for k in grid if grid[k] == "E")

    path = dfs(grid, S, E)

    cheats = defaultdict(set)
    vis = set()

    for i in trange(len(path) - 1):
        for j in range(i + 2, len(path)):
            p1, p2 = path[i], path[j]
            dist = manh(p1, p2)
            save = j - i - dist
            if save > 0 and 2 <= dist <= 20 and (p1, p2) not in vis:
                cheats[(save, dist)].add((p1, p2))
                vis.add((p1, p2))

    p1 = sum(len(pairs) for (s, d), pairs in cheats.items() if s >= 100 and d == 2)
    p2 = sum(len(pairs) for (s, d), pairs in cheats.items() if s >= 100)

    return p1, p2


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
