from pathlib import Path
from typing import TextIO

import networkx as nx


def dfs(src, grid, vis):
    st = [src]
    region = []
    while st:
        cur = st.pop()
        if vis[cur]:
            continue
        vis[cur] = True
        region.append(cur)

        for dz in [-1, 1, -1j, 1j]:
            nxt = cur + dz
            if nxt in grid and grid[nxt] == grid[src] and not vis[nxt]:
                st.append(nxt)

    return region


def get_boundary(region, grid):
    boundary = set()
    for node in region:
        for dz in [-1, 1j, 1, -1j]:
            n = node + dz
            if n not in grid or (n in grid and grid[n] != grid[node]):
                boundary.add((node, dz))

    return boundary


def peri1(region, grid):
    return len(get_boundary(region, grid))


def peri2(region, grid):
    boundary = get_boundary(region, grid)
    g = nx.Graph()

    for cur, dcur in boundary:
        g.add_node((cur, dcur))
        for z, dz in boundary - {cur}:
            if z.real == cur.real and abs(z.imag - cur.imag) == 1 and dcur == dz:
                g.add_edge((cur, dcur), (z, dz))
            if z.imag == cur.imag and abs(z.real - cur.real) == 1 and dcur == dz:
                g.add_edge((cur, dcur), (z, dz))

    return sum(1 for _ in nx.connected_components(g))


def fence_cost(grid, region_peri_func):
    vis = {k: False for k in grid}
    tot = 0

    while not all(vis.values()):
        src = [k for k in vis if not vis[k]].pop()
        region = dfs(src, grid, vis)
        area = len(region)
        peri = region_peri_func(region, grid)
        tot += area * peri

    return tot


def solve(fp: TextIO):
    grid = {
        complex(i, j): x
        for i, row in enumerate(fp.read().strip().split("\n"))
        for j, x in enumerate(row)
    }

    p1 = fence_cost(grid, peri1)
    p2 = fence_cost(grid, peri2)

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 1930
    assert p2 == 1206


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
