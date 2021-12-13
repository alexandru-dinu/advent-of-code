import sys

import matplotlib.pyplot as plt
import networkx as nx


def main():
    with open(sys.argv[1]) as fp:
        g = [l.strip().split("-") for l in fp.readlines()]

    G = nx.Graph()

    for u, v in g:
        G.add_edge(u, v)

    nodes = list(G.nodes)

    color_map = ["#9e9e9e"] * len(nodes)
    color_map[nodes.index("start")] = "#dc322f"
    color_map[nodes.index("end")] = "#00cc00"
    for i, node in enumerate(nodes):
        if node.upper() == node:
            color_map[i] = "#268bd2"

    nx.draw(G, pos=None, with_labels=True, node_color=color_map, node_size=1000)
    plt.show()


if __name__ == "__main__":
    main()
