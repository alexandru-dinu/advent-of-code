import heapq
from collections import defaultdict, deque
from pathlib import Path
from typing import TextIO

from tqdm import tqdm


def get_obstacles(fp: TextIO):
    out = []
    for row in fp.readlines():
        x, y = map(int, row.split(","))
        out.append(complex(x, y))
    return out


def recompose_path(parents, src):
    path = [src]
    while (nxt := parents[path[-1]]) is not None:
        path.append(nxt)

    return path[::-1]


def in_bounds(z, dim):
    return 0 <= z.real < dim and 0 <= z.imag < dim


def manh(p, q):
    return abs(p.real - q.real) + abs(p.imag - q.imag)


def astar(obstacles, dim):
    start = complex(0, 0)
    end = complex(dim - 1, dim - 1)

    pq = []
    heapq.heappush(pq, (0, start.real, start.imag))

    cost = defaultdict(lambda: float("inf"))
    cost[start] = 0

    parents = defaultdict(lambda: None)

    while pq:
        prio, x, y = heapq.heappop(pq)
        cur = complex(x, y)

        if cur == end:
            return recompose_path(parents, cur)

        for dz in [1, -1, 1j, -1j]:
            nxt = cur + dz
            if not in_bounds(nxt, dim) or nxt in obstacles:
                continue

            if cost[cur] + 1 < cost[nxt]:  # cost of a new node is by default INF (unvisited node)
                cost[nxt] = cost[cur] + 1
                heapq.heappush(pq, (cost[nxt] + manh(nxt, end), nxt.real, nxt.imag))
                parents[nxt] = cur

    return None


def dfs(obstacles, dim):
    start = complex(0, 0)
    end = complex(dim - 1, dim - 1)

    st = [start]

    cost = defaultdict(lambda: float("inf"))
    cost[start] = 0

    parents = defaultdict(lambda: None)

    best = float("inf")
    path = None

    while st:
        cur = st.pop()

        if cur == end:
            if cost[cur] < best:
                best = cost[cur]
                path = recompose_path(parents, cur)

        for dz in [1, -1, 1j, -1j]:
            nxt = cur + dz
            if not in_bounds(nxt, dim) or nxt in obstacles:
                continue

            if cost[cur] + 1 < cost[nxt]:  # cost of a new node is by default INF (unvisited node)
                cost[nxt] = cost[cur] + 1
                st.append(nxt)
                parents[nxt] = cur

    return path


def bfs(obstacles, dim):
    start = complex(0, 0)
    end = complex(dim - 1, dim - 1)

    st = deque([start])

    cost = defaultdict(lambda: float("inf"))
    cost[start] = 0

    parents = defaultdict(lambda: None)

    best = float("inf")
    path = None

    while st:
        cur = st.popleft()

        if cur == end:
            if cost[cur] < best:
                best = cost[cur]
                path = recompose_path(parents, cur)

        for dz in [1, -1, 1j, -1j]:
            nxt = cur + dz
            if not in_bounds(nxt, dim) or nxt in obstacles:
                continue

            if cost[cur] + 1 < cost[nxt]:  # cost of a new node is by default INF (unvisited node)
                cost[nxt] = cost[cur] + 1
                st.append(nxt)
                parents[nxt] = cur

    return path


# SEARCH_FUNC = astar
SEARCH_FUNC = bfs
# SEARCH_FUNC = dfs  # slowest


def find_first_blocking(obstacles, dim, lim):
    with tqdm(total=len(obstacles) - lim) as pbar:
        while SEARCH_FUNC(set(cur := obstacles[:lim]), dim) is not None:
            pbar.update(1)
            lim += 1

    return cur[-1]


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        obs = get_obstacles(fp)

    assert 22 == len(SEARCH_FUNC(set(obs[:12]), dim=7)) - 1
    assert complex(6, 1) == find_first_blocking(obs, dim=7, lim=13)


def main():
    with open(0) as fp:
        obs = get_obstacles(fp)

    p1 = len(SEARCH_FUNC(set(obs[:1024]), dim=71)) - 1
    p2 = find_first_blocking(obs, dim=71, lim=1025)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
