import math
import random
import re
from pathlib import Path
from typing import TextIO

import matplotlib.pyplot as plt
import networkx as nx


def read_graph(fp):
    g = nx.Graph()

    for line in fp:
        x, *ys = re.findall(r"[a-z]+", line)
        for y in ys:
            g.add_edge(x, y, weight=1)

    return g


def show_graph(g):
    nx.draw(g, with_labels=True, node_size=50)
    plt.show()


def conn_comp(g) -> list[set]:
    def dfs(g, start) -> set:
        st = [start]
        vis = set()

        while st:
            cur = st.pop()
            if cur in vis:
                continue
            vis.add(cur)

            for nxt in g.neighbors(cur):
                if nxt not in vis:
                    st.append(nxt)

        return vis

    ccs = []
    nodes = set(g.nodes)

    while nodes:
        # start dfs from a random unvisited node
        cc = dfs(g, start=next(iter(nodes)))
        ccs.append(cc)
        nodes -= cc

    return ccs


def solve_min_cut(g):
    # there is a min cut of size exactly 3 which partitions the graph
    nodes = list(g.nodes)
    cut = float("inf")

    while cut != 3:
        s, t = random.sample(nodes, k=2)
        cut, ccs = nx.minimum_cut(g, s, t, capacity="weight")
    assert len(ccs) == 2
    assert s in ccs[0] and t in ccs[1]

    return math.prod(map(len, ccs))


def solve_centrality(g):
    # top 3 edges sorted by betweenness centrality are the ones to be removed
    bc = sorted(
        nx.edge_betweenness_centrality(g).items(), key=lambda kv: kv[1], reverse=True
    )
    for (u, v), _ in bc[:3]:
        g.remove_edge(u, v)

    return math.prod(map(len, conn_comp(g)))


def solve(fp: TextIO):
    g = read_graph(fp)

    r1 = solve_min_cut(g)
    r2 = solve_centrality(g)
    assert r1 == r2

    return r1


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        g = read_graph(fp)
        solve_min_cut(g) == solve_centrality(g) == 54


def main():
    with open(0) as fp:
        print(f"Result: {solve(fp)}")


if __name__ == "__main__":
    main()
