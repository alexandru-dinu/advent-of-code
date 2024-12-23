from collections import defaultdict
from pathlib import Path
from typing import TextIO

import matplotlib.pyplot as plt
import networkx as nx


def cliques_size_3(graph):
    cliq3 = set()

    for n1 in graph:
        for n2 in graph[n1]:
            for n3 in graph[n2]:
                if n1 in graph[n3]:
                    cliq3.add(tuple(sorted([n1, n2, n3])))

    # alt. but slower
    # for u, v, z in combinations(graph, 3):
    #     if v in graph[u] and z in graph[v] and u in graph[z]:
    #         cliq3.add((u, v, z))

    return cliq3


def max_clique(graph):

    def _inner(cur, rem):
        if not rem:
            return cur
        while rem:
            u = rem.pop()
            ok = True
            for v in cur:
                if not (v in graph[u] or u in graph[v]):
                    ok = False
                    break
            if ok:
                cur |= {u}
            return _inner(cur, rem)

    largest = ()

    for v in graph:
        res = _inner(cur={v}, rem=set(graph.keys()) - {v})
        res = tuple(sorted(res))
        if len(res) > len(largest):
            largest = res

    return largest


def solve(fp: TextIO):
    edges = [x.split("-") for x in fp.read().strip().split("\n")]

    nxg = nx.Graph()
    nxg.add_edges_from(edges)

    if 0:
        nx.draw_circular(nxg, with_labels=True)
        plt.show()

    graph = defaultdict(list)
    for u, v in edges:
        graph[u].append(v)
        graph[v].append(u)

    p1 = sum(any(c[0] == "t" for c in g) for g in cliques_size_3(graph))
    p2 = ",".join(max_clique(graph))

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 7
    assert p2 == "co,de,ka,ta"


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
