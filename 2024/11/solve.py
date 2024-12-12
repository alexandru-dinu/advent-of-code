from collections import Counter, defaultdict


def step(xs, n_steps):
    # nodes[node][level] = count
    nodes = defaultdict(Counter)

    def add(old, new, level):
        nodes[new][level] += nodes[old][level - 1]

    def get_level_nodes(target):
        for node in nodes:
            for level, count in nodes[node].items():
                if level == target:
                    yield node, count

    for x in xs:
        nodes[x][0] += 1

    for level in range(1, n_steps + 1):
        ns = [n for n, _ in get_level_nodes(level - 1)]

        for old in ns:
            if old == 0:
                add(old=old, new=1, level=level)

            elif (n := len(s := str(old))) % 2 == 0:
                add(old=old, new=int(s[: n // 2]), level=level)
                add(old=old, new=int(s[n // 2 :]), level=level)

            else:
                add(old=old, new=old * 2024, level=level)

    return sum(c for _, c in get_level_nodes(n_steps))


def solve(fp):
    xs = [int(x) for x in fp.read().strip().split(" ")]

    p1 = step(xs, n_steps=25)
    p2 = step(xs, n_steps=75)

    return p1, p2


def test_example():
    xs = [125, 17]
    assert step(xs, 25) == 55312
    assert step(xs, 75) == 65601038650482


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
