from collections import deque
from pathlib import Path
from typing import TextIO

import numpy as np


def get_inp(fp: TextIO) -> np.ndarray:
    return np.array([list(x.strip()) for x in fp])


def nei(mat, i, j):
    h, w = mat.shape

    for di, dj in [(0, 1), (0, -1), (-1, 0), (1, 0)]:
        # real
        ri, rj = i + di, j + dj
        # next
        ni, nj = ri % h, rj % w

        if mat[ni, nj] == "#":
            continue

        yield ri, rj


def bfs(mat: np.ndarray, start: tuple, limit: int):
    q = deque([(0, start)])

    while True:
        nq = deque([])

        assert len({i for i, _ in q}) == 1, q
        vis = set()
        while q:
            it, cur = q.popleft()
            if it == limit:
                return len(q) + 1

            for nxt in nei(mat, *cur):
                if nxt in vis:
                    continue
                nq.append((it + 1, nxt))
                vis.add((nxt))

        q = nq


def solve(fp: TextIO):
    mat = get_inp(fp)

    start = np.argwhere(mat == "S")[0]

    p1 = bfs(mat, tuple(start), limit=64)

    xs = np.arange(3)
    ys = [bfs(mat, tuple(start), limit=(65 + k * 131)) for k in xs]
    m, d = divmod(26501365, len(mat))
    p2 = round(np.polyval(np.polyfit(xs, ys, deg=2), m))

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        mat = get_inp(fp)

    start = tuple(np.argwhere(mat == "S")[0])

    for l, s in [(6, 16), (10, 50), (50, 1594), (100, 6536)]:
        assert bfs(mat, start, limit=l) == s


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
