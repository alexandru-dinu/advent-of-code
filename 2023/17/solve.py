from collections import defaultdict
from dataclasses import dataclass
from heapq import heappop, heappush
from pathlib import Path
from typing import TextIO

import numpy as np


def get_inp(fp: TextIO):
    return np.array([list(map(int, x.strip())) for x in fp])


@dataclass
class Cell:
    pos: complex
    ori: complex
    fuel: int

    def tup(self):
        return (*self.ij(), self.ori, self.fuel)

    def ij(self):
        return int(self.pos.imag), int(self.pos.real)

    def __hash__(self):
        return hash(self.tup())

    def __lt__(self, other):
        return self.ij() < other.ij()


def pathfind_bot_right(mat, src, nei_func):
    """
    Find shortest path from `src` to bottom right of the `mat`.
    Using a custom func to get neighbours for a given state.
    """
    h, w = mat.shape

    g = defaultdict(lambda: np.inf)
    frontier = []
    parent = {}

    g[src] = 0
    heappush(frontier, (g[src], src))
    parent[src] = None

    while frontier:
        cost, cur = heappop(frontier)

        # found path, reconstruct path
        if cur.ij() == (h - 1, w - 1):
            xs = [cur]
            while xs[-1]:
                xs.append(parent[xs[-1]])
            xs = xs[::-1][1:]
            return cost, xs

        for nei in nei_func(mat, cur):
            alt = g[cur] + mat[nei.ij()]
            if alt < g[nei]:
                parent[nei] = cur
                g[nei] = alt
                heappush(frontier, (g[nei], nei))

    assert False, "unreachable"


def in_bounds(z, h, w):
    return 0 <= z.imag < h and 0 <= z.real < w


def nei1(mat, cell, max_fuel=3):
    """
    Must move n <= `max_fuel` cells in the same direction.
    Reset when changing direction.
    """
    h, w = mat.shape

    for dz in [-1, 1, 1j, -1j]:
        nz = cell.pos + dz

        if not in_bounds(nz, h, w):
            continue

        # can't go back
        if cell.ori + dz == 0:
            continue

        # different orientation, reset fuel
        if dz != cell.ori:
            yield Cell(pos=nz, ori=dz, fuel=max_fuel)
        # same orientation, decrement fuel
        else:
            f = cell.fuel - 1
            if f == 0:
                continue
            yield Cell(pos=nz, ori=dz, fuel=f)


def nei2(mat, cell):
    """
    Must move 4 <= n <= 10 cells in the same direction.
    Reset when changing direction.
    """
    h, w = mat.shape

    f = cell.fuel

    # start a new streak only if the end is still in bounds
    if f == 10:
        nz = cell.pos + 3 * cell.ori
        if not in_bounds(nz, h, w):
            return
        yield Cell(pos=cell.pos + cell.ori, ori=cell.ori, fuel=f - 1)

    # inside the streak
    elif f > 7:
        yield Cell(pos=cell.pos + cell.ori, ori=cell.ori, fuel=f - 1)

    # free to change direction
    else:
        yield from nei1(mat, cell, max_fuel=10)


def plot(mat, path):
    import matplotlib.pyplot as plt
    import seaborn as sns

    ax = sns.heatmap(mat, cmap="Blues")
    for c in path:
        i, j = c.ij()
        c = {1j: "v", -1j: "^", -1: "<", 1: ">"}.get(c.ori)
        ax.text(j + 0.5, i + 0.5, c, color="r", va="center", ha="center")
    plt.show()


def solve(fp: TextIO):
    mat = get_inp(fp)

    cost, _ = pathfind_bot_right(
        mat, src=Cell(pos=0 + 0j, ori=1, fuel=3), nei_func=nei1
    )
    p1 = cost

    cost, path = pathfind_bot_right(
        mat, src=Cell(pos=0 + 0j, ori=1, fuel=10), nei_func=nei2
    )
    p2 = cost
    # plot(mat, path)

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        assert solve(fp) == (102, 94)

    with open(Path(__file__).parent / "example2") as fp:
        _, p2 = solve(fp)
        assert p2 == 71


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
