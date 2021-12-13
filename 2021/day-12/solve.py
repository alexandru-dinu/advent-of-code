import sys
from collections import defaultdict
from typing import *

import numpy as np

Graph = Dict[str, List[str]]


def node_type(node: str) -> str:
    if node in ["start", "end"]:
        return "special"

    if node.islower():
        return "small"

    return "big"


def init_can_visit(graph: Graph) -> Dict[str, float]:
    can_visit = {}

    for node in graph:
        if node == "start":
            can_visit[node] = 0
        elif node.isupper():
            can_visit[node] = np.inf
        else:
            can_visit[node] = 1

    return can_visit


def dfs(graph, cur, target, acc, paths, can_visit):
    if cur == target:
        paths.add(tuple(acc))
        return

    for nei in set(graph[cur]):
        if can_visit[nei] == 0:
            continue

        can_visit[nei] -= 1
        dfs(graph, nei, target, [*acc, nei], paths, can_visit)
        can_visit[nei] += 1


def part1(graph: Graph):
    """
    Count paths that visit small caves at most once.
    """
    paths = set()

    dfs(
        graph,
        cur="start",
        target="end",
        acc=["start"],
        paths=paths,
        can_visit=init_can_visit(graph),
    )

    return len(paths)


def part2(graph: Graph):
    """
    Count paths that visit a single small cave <=2, other small caves <=1, big caves *.
    """
    paths = set()

    can_visit = init_can_visit(graph)

    for node in graph:
        if node_type(node) != "small":
            continue

        can_visit[node] += 1
        dfs(
            graph,
            cur="start",
            target="end",
            acc=["start"],
            paths=paths,
            can_visit=can_visit,
        )
        can_visit[node] -= 1

    return len(paths)


def main():
    with open(sys.argv[1]) as fp:
        edges = [l.strip().split("-") for l in fp.readlines()]

    graph = defaultdict(lambda: [])
    for u, v in edges:
        graph[u].append(v)
        graph[v].append(u)

    print("Part 1:", part1(graph))
    print("Part 2:", part2(graph))


if __name__ == "__main__":
    main()
