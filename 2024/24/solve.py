import random
from pathlib import Path
from typing import TextIO

import matplotlib.pyplot as plt
import networkx as nx
from bidict import bidict


def to_num(mem, pre):
    return sum(v << int(k[1:]) for k, v in mem.items() if k[0] == pre)


def draw(graph):
    random.seed(192391)

    xs = sorted((node for node in graph.nodes if node.startswith("x")), key=lambda x: int(x[1:]))
    ys = sorted((node for node in graph.nodes if node.startswith("y")), key=lambda x: int(x[1:]))
    zs = sorted((node for node in graph.nodes if node.startswith("z")), key=lambda x: int(x[1:]))
    rest = [node for node in graph.nodes if node not in xs + ys + zs]

    pos = {}
    x_offset = 0
    for i, nodes in enumerate([xs + ys, rest, zs]):
        for j, node in enumerate(nodes):
            dx = -abs(random.random() - 1) if nodes is rest else 0
            dy = 0.5 * random.random() if nodes is rest else 0
            pos[node] = (x_offset + dx, -j + dy)
        x_offset += 2

    cm = {"x": "salmon", "y": "lightblue", "z": "red"}
    plt.figure(figsize=(15, 20))
    nx.draw(
        graph,
        pos,
        with_labels=True,
        node_color=[cm.get(x[0], "gray") for x in graph.nodes],
        edge_color=[cm.get(u[0], "black") for u, _ in graph.edges],
        node_size=300,
        font_size=10,
        font_family="monospace",
        arrowsize=10,
    )
    plt.show()


def adder_nbit(n1, n2, n):
    assert 0 <= n1 < 2**n
    assert 0 <= n2 < 2**n

    c = 0
    res = 0
    for k in range(n):
        x, y = (n1 >> k) & 1, (n2 >> k) & 1

        t1 = x ^ y
        z = t1 ^ c
        t2 = x & y
        t3 = t1 & c
        c = t2 | t3

        res |= z << k

    res |= c << n

    return res


def write_diagram(rules):
    from python_mermaid.diagram import Link, MermaidDiagram, Node

    nodes = []
    links = []
    seen = set()
    k = 0
    for k, (r, (x, o, y)) in enumerate(rules.items()):
        nx = Node(x)
        ny = Node(y)
        nr = Node(r, shape="circle" if r.startswith("z") else "normal")
        no = Node(f"{o}{k}", o, shape={"XOR": "rhombus", "AND": "hexagon", "OR": "trapezoid"}[o])
        for n in [nx, ny, nr, no]:
            if n.id in seen:
                continue
            nodes.append(n)
            seen.add(n.id)
        links.append(Link(nx, no))
        links.append(Link(ny, no))
        links.append(Link(no, nr))

    diag = MermaidDiagram(title="AOC24", nodes=nodes, links=links)
    with open("out.mermaid", "wt") as fp:
        print(diag, file=fp)


def rev_eng(rules):
    errs = []

    assert rules.inverse[("x00", "XOR", "y00")] == "z00"
    c = rules.inverse[("x00", "AND", "y00")]

    def try_key(x, op, y):
        if (x, op, y) not in rules.inverse:
            x, y = y, x
        k = (x, op, y)
        return k, rules.inverse[(x, op, y)]

    for i in range(1, 45):
        k = f"{i:02d}"
        print(f"\n>>> {i}, carry: {c}")

        x = f"x{k}"
        y = f"y{k}"
        z = f"z{k}"

        _, t1 = try_key(x, "XOR", y)  # always exists, adds current bits x, y
        print(f"{x} XOR {y} = {t1}")

        if i == 33:
            c = "gfm"
            errs.extend(["z32", c])
        if i == 38:
            errs.append(t1)
            t1 = "qjd"
            errs.append(t1)

        _, z_new = try_key(t1, "XOR", c)
        print(f"{t1} XOR {c} = {z_new}")

        if z_new != z:
            print("!!!", z, z_new)

        _, t2 = try_key(x, "AND", y)
        print(f"{x} AND {y} = {t2}")

        _, t3 = try_key(t1, "AND", c)
        print(f"{t1} AND {c} = {t3}")

        if i == 8:
            assert z_new == "cdj"
            t2 = z_new
            errs.extend(["z08", t2])
        if i == 16:
            assert z_new == "mrb"
            t3 = z_new
            errs.extend(["z16", t3])
        if i == 38:
            t2 = "dhm"

        _, c_new = try_key(t2, "OR", t3)
        print(f"{t2} OR {t3} = {c_new}")

        c = c_new

    return ",".join(sorted(errs))


def solve(fp: TextIO):
    lim = 6
    for x in range(1 << lim):
        for y in range(1 << lim):
            assert x + y == adder_nbit(x, y, n=lim)

    mem, rules = fp.read().strip().split("\n\n")
    mem = {x[0]: int(x[1]) for row in mem.split("\n") if (x := row.split(": "))}
    rules = bidict({x[-1]: (x[0], x[1], x[2]) for row in rules.split("\n") if (x := row.split())})

    write_diagram(rules)

    f = {"XOR": "^", "AND": "&", "OR": "|"}

    # graph = nx.DiGraph()
    # for res, (x, op, y) in rules.items():
    #     graph.add_edge(x, res)
    #     graph.add_edge(y, res)
    # draw(graph)

    while set(rules) - set(mem):
        for res, (x, op, y) in rules.items():
            if res not in mem and x in mem and y in mem:
                mem[res] = eval(f"{mem[x]} {f[op]} {mem[y]}")

    x = to_num(mem, pre="x")
    y = to_num(mem, pre="y")
    z = to_num(mem, pre="z")
    # print(f"{x} + {y} = {z} ({x + y == z})")

    p1 = z
    p2 = rev_eng(rules)

    return p1, p2


def main():
    with open(Path(__file__).parent / "input") as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
