import sys
from collections import defaultdict
from heapq import heappop, heappush

import numpy as np


def neighbours(grid, node: tuple):
    nei = []
    i, j = node
    for di, dj in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        i_, j_ = i + di, j + dj
        if 0 <= i_ < grid.shape[0] and 0 <= j_ < grid.shape[1]:
            nei += [(i_, j_)]
    return nei


def reconstruct_path(parent, cur):
    path = [cur]
    while cur in parent:
        cur = parent[cur]
        path = [cur, *path]
    return path


def pathfind(risk: np.ndarray, src: tuple, tgt: tuple, use_heu: bool = True):
    if use_heu:
        # use manhattan distance as heuristic function
        heu = lambda x: abs(x[0] - tgt[0]) + abs(x[1] - tgt[1])
    else:
        heu = lambda x: 0

    g = defaultdict(lambda: np.inf)
    g[src] = 0

    f = defaultdict(lambda: np.inf)
    f[src] = g[src] + heu(src)

    frontier = []
    heappush(frontier, (f[src], src))
    visited = {src}

    parent = {}

    while frontier:
        _, cur = heappop(frontier)
        visited.remove(cur)

        if cur == tgt:
            return reconstruct_path(parent, cur)

        for nei in neighbours(risk, cur):
            tentative = g[cur] + risk[nei]
            if tentative < g[nei]:
                parent[nei] = cur
                g[nei] = tentative
                f[nei] = tentative + heu(nei)
                if nei not in visited:
                    visited.add(nei)
                    heappush(frontier, (f[nei], nei))

    assert False, "unreachable"


def make_tiled(risk, h_rep: int, w_rep: int):
    h, w = risk.shape
    tile = np.zeros((h * h_rep, w * w_rep), dtype=int)

    tile[:h, :w] = risk

    I = lambda i: slice(i * h, (i + 1) * h)
    J = lambda j: slice(j * w, (j + 1) * w)

    for j in range(1, w_rep):
        tile[I(0), J(j)] = tile[I(0), J(j - 1)] % 9 + 1

    for i in range(1, h_rep):
        tile[I(i), J(0)] = tile[I(i - 1), J(0)] % 9 + 1

    for i in range(1, h_rep):
        for j in range(1, w_rep):
            tile[I(i), J(j)] = tile[I(i - 1), J(j)] % 9 + 1

    return tile


def path_cost(risk):
    src = (0, 0)
    tgt = (risk.shape[0] - 1, risk.shape[1] - 1)

    path = pathfind(risk, src, tgt, use_heu=True)

    return sum(risk[n] for n in path[1:])


def main():
    risk = np.stack(
        [np.fromiter(x, dtype=int) for x in np.loadtxt(sys.argv[1], dtype=str)]
    )

    print("Part 1:", path_cost(risk))
    print("Part 2:", path_cost(make_tiled(risk, h_rep=5, w_rep=5)))


if __name__ == "__main__":
    main()
