from __future__ import annotations

import sys
from collections import defaultdict


def dfs(graph: dict[str, set[str]], acc: list[str], vis: set[str], twice: bool) -> set:
    cur = acc[-1]

    if cur == "end":
        return {tuple(acc)}

    paths = set()

    for nei in graph[cur] - vis:
        if nei.isupper():
            # don't mark big node as visited
            paths |= dfs(graph, [*acc, nei], vis, twice)
        else:
            # visit small node, passing "twice" further
            paths |= dfs(graph, [*acc, nei], vis | {nei}, twice)
            if twice:
                # allow small node to be visited twice
                paths |= dfs(graph, [*acc, nei], vis, twice=False)

    return paths


def main():
    with open(sys.argv[1]) as fp:
        edges = [l.strip().split("-") for l in fp.readlines()]

    graph = defaultdict(lambda: set())
    for u, v in edges:
        graph[u].add(v)
        graph[v].add(u)

    print("Part 1:", len(dfs(graph, acc=["start"], vis={"start"}, twice=False)))
    print("Part 2:", len(dfs(graph, acc=["start"], vis={"start"}, twice=True)))


if __name__ == "__main__":
    main()
