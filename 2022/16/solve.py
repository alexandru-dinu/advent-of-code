import functools
import re

import matplotlib.pyplot as plt
import networkx as nx
import numpy as np
from tqdm import tqdm, trange

pattern = re.compile(
    r"Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? (.*)"
)


class Graph(nx.DiGraph):
    def __init__(self, xs):
        super(Graph, self).__init__()

        for src, flow, dest in xs:
            self.add_node(src, flow=flow)
            for d in dest:
                self.add_edge(src, d, weight=1)

        self.shortest = dict(nx.all_pairs_shortest_path_length(self))

    def viz(self):
        plt.figure(dpi=200)
        nx.draw(
            self,
            pos=nx.spring_layout(self, seed=42),
            node_color=[
                "red" if d["flow"] == 0 else "blue" for _, d in self.nodes(data=True)
            ],
            with_labels=True,
            labels={n: f"{n}\n{d['flow']}" for n, d in self.nodes(data=True)},
            font_size=10,
            node_size=1000,
            font_color="white",
            font_family="monospace",
        )
        plt.show()


def parse(line):
    src, flow, dest = pattern.match(line).groups()
    flow = int(flow)
    dest = dest.split(", ")

    return src, flow, dest


@functools.cache
def traverse(g: Graph, cur: str, visited: frozenset[str], time_budget: int):
    max_flow = 0
    reachable = set(g.shortest[cur]) - visited

    for u in reachable:
        # valve is not openable
        if g.nodes[u]["flow"] == 0:
            continue

        time_taken = 1 + g.shortest[cur][u]

        # can't open this valve in time
        if time_taken > time_budget:
            continue

        # valve is both openable and reachable => open it and continue with the rest
        dt = time_budget - time_taken
        rest = traverse(g, u, visited | {u}, dt)
        max_flow = max(max_flow, dt * g.nodes[u]["flow"] + rest)

    return max_flow


def dynamic_programming(
    g: Graph, node2idx: dict[str, int], openable: set[str], time_budget: int
):
    assert len(node2idx) == len(g.nodes)

    valve_states = 2 ** len(openable)
    vidx = {n: i for i, n in enumerate(openable)}

    # state = (time budget, current node, current valve states: 1 = openable)
    dp = np.zeros((time_budget, len(g.nodes), valve_states), dtype=np.uint32)

    for t in trange(1, time_budget, position=0, leave=False):
        for n, i in tqdm(node2idx.items(), position=1, leave=False):
            for v in range(valve_states):
                cur = dp[t, i, v]
                # in one step we can do two actions:

                # 1/ open a valve
                # if the current node is an openable valve and has not been opened yet
                # check if we're better off opening it
                # e.g. if valves' state is given by 1[1]0
                #                            nodes: a b c
                # then prev valves' states are: 0[0]0, 0[0]1, 1[0]0, 1[0]1
                # so we check if we get a better score by also opening valve b
                # 0[0]0 => 0[1]0: no valves were previously opened, open b
                # 0[0]1 => 0[1]1: valve c was previously opened, open b
                # 1[0]0 => 1[1]0: valve a was previously opened, open b
                # 1[0]1 => 1[1]1: valve a and c were previously opened, open b
                if n in openable and 0 != v & (1 << vidx[n]):
                    prev = dp[t - 1, i, v ^ (1 << vidx[n])]
                    cur = max(cur, prev + t * g.nodes[n]["flow"])

                # 2/ move to a neighboring node
                for u in g.neighbors(n):
                    cur = max(cur, dp[t - 1, node2idx[u], v])

                dp[t, i, v] = cur

    return dp


def main():
    with open(0) as fp:
        xs = [x.strip() for x in fp]

    g = Graph([parse(x) for x in xs])
    # g.viz()

    t = 30

    print("Part 1:", traverse(g, cur="AA", visited=frozenset(["AA"]), time_budget=t))

    node2idx = {n: i for i, n in enumerate(g.nodes)}
    openable = {n for n, d in g.nodes(data=True) if d["flow"] > 0}
    dp = dynamic_programming(g, node2idx, openable, time_budget=t)
    print("Part 1 (dp):", dp[t - 1, node2idx["AA"], :].max())

    # disjoint valve states for human and elephant
    valve_states = 2 ** len(openable)
    part2 = 0
    for h_valves in range(valve_states // 2):
        e_valves = (valve_states - 1) ^ h_valves
        h = dp[t - 1 - 4, node2idx["AA"], h_valves]
        e = dp[t - 1 - 4, node2idx["AA"], e_valves]
        part2 = max(part2, h + e)

    print("Part 2 (dp):", part2)


if __name__ == "__main__":
    main()
