from collections import Counter, deque
from typing import Iterable

import numpy as np

Coord = tuple[int, int]


def neighbours(xss: np.ndarray, c: Coord) -> Iterable[Coord]:
    h, w = xss.shape
    ci, cj = c

    for di, dj in ((0, 1), (0, -1), (1, 0), (-1, 0)):
        ni, nj = ci + di, cj + dj

        if not (0 <= ni < h and 0 <= nj < w):
            continue

        if not xss[ni, nj] - xss[ci, cj] <= 1:
            continue

        yield ci + di, cj + dj


def bfs(xss: np.ndarray, start: list[Coord], end: Coord) -> Counter:
    q = deque(start)
    dist = Counter()
    for c in start:
        dist[c] = 0

    while q:
        cur = q.popleft()

        for nxt in neighbours(xss, cur):
            if nxt in dist:
                continue

            dist[nxt] = dist[cur] + 1
            q.append(nxt)

            if nxt == end:
                return dist

    assert False, "unreachable"


def main():
    with open(0) as fp:
        xss = np.array([list(map(ord, xs.strip())) for xs in fp])

    S = next(zip(*np.where(xss == ord("S"))))
    E = next(zip(*np.where(xss == ord("E"))))

    xss = xss - ord("a")
    xss[S] = 0
    xss[E] = ord("z") - ord("a")

    dist = bfs(xss, start=[S], end=E)
    print("Part 1:", dist[E])

    dist = bfs(xss, start=list(zip(*np.where(xss == 0))), end=E)
    print("Part 2:", dist[E])


if __name__ == "__main__":
    main()
