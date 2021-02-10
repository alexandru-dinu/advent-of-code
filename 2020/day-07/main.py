import sys
import re
from typing import List
from collections import defaultdict

REGEX = re.compile(r"(\d) (\w+ \w+) (bags?)")


def parse_line(x: str) -> tuple:
    src, dst = map(lambda l: l.strip(), x.split("bags contain"))

    if dst == "no other bags.":
        return src, []

    dst = [REGEX.match(l.strip()).groups() for l in dst.split(",")]
    dst = [(int(l[0]), l[1]) for l in dst]  # (n, color)

    return src, dst


def transpose(graph: dict) -> dict:
    t_graph = defaultdict(lambda: [])

    for (src, dsts) in graph.items():
        for (n, dst) in dsts:
            t_graph[dst].append((n, src))

    return t_graph


def count_parents(graph: dict, from_node: str) -> int:
    """Part 1"""

    def _inner(graph, node, acc):
        if not graph[node]:
            return

        for (n, nxt) in graph[node]:
            acc.add(nxt)
            _inner(graph, nxt, acc)

    acc = set()
    _inner(graph, from_node, acc)

    return len(acc)


def count_children(graph: dict, from_node: str) -> int:
    """
    Part 2
    f([])   = 0
    f(node) = Σ_(#neigh) n_i * (1 + f(neigh_i))
    """

    if not graph[from_node]:
        return 0

    s = 0
    for (n, nxt) in graph[from_node]:
        s += n * (1 + count_children(graph, nxt))

    return s


if __name__ == "__main__":
    with open(sys.argv[1], "rt") as fp:
        graph = dict([parse_line(l.strip()) for l in fp.readlines()])

    # make each inner bag point to parent
    t_graph = transpose(graph)
    print(f'Part 1: {count_parents(t_graph, from_node="shiny gold")}')
    print(f'Part 2: {count_children(graph, from_node="shiny gold")}')
