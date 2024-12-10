###### 09
from copy import deepcopy
from pathlib import Path

"""
Some sort of run-length encoding

TODO:
    - don't keep 3 items, use just (x,y) and if y is None, then it's empty
"""


def debug(d):
    return
    out = []
    for x, s, y in d:
        if not s:
            out.append(f"{x}:{y}")
        else:
            out.append(f"[{x}]:{y}")
    print(" ".join(out))


def parse(fp):
    d = []
    k = 0
    for i, x in enumerate(fp.read().strip()):
        free = i % 2 == 1
        d.append((int(x), free, k if not free else None))
        if free:
            k += 1

    return d


def checksum(d):
    tot = 0
    i = 0
    for x, empty, y in d:
        if not empty:
            for j in range(x):
                tot += i * y
                i += 1
        else:
            assert y is None
            i += x
    return tot


def solve1(d):
    debug(d)
    # [t]rue = empty
    # [f]alse = used

    # t = 1
    # f = len(d) - 1 - (len(d) % 2 == 0)

    # TODO: replace w/ proper inc/dec
    t = min(i for i, (x, s, _) in enumerate(d) if s)
    f = max(i for i, (x, s, _) in enumerate(d) if not s)

    while t < f and 0 <= t < len(d) and 0 <= f < len(d):
        if d[t][0] == d[f][0]:
            d[t], d[f] = d[f], d[t]

        elif d[t][0] > d[f][0]:
            diff = d[t][0] - d[f][0]
            d[t] = (d[f][0], False, d[f][2])  # used
            d[f] = (d[f][0], True, None)  # clear space at the end
            d.insert(t + 1, (diff, True, None))  # rem empty space

        else:
            diff = d[f][0] - d[t][0]
            d[t] = (d[t][0], False, d[f][2])
            d[f] = (diff, False, d[f][2])
            d.insert(f + 1, (d[t][0], True, None))

        t = min(i for i, (x, s, _) in enumerate(d) if s)
        f = max(i for i, (x, s, _) in enumerate(d) if not s)
        debug(d)

    # all used space then all free space
    for i, (_, s, _) in enumerate(d):
        if s:
            for j in range(i, len(d)):
                assert d[j][1]

    return checksum(d)


def solve2(d):
    debug(d)

    f = max(i for i, (x, s, _) in enumerate(d) if not s)
    to_move = {y for (_, empty, y) in d if not empty}

    for to_move in sorted(to_move, reverse=True):
        cur = [f for f, (x, empty, y) in enumerate(d) if not empty and y == to_move]
        assert len(cur) == 1
        f = cur[0]
        cur = d[f]

        try:
            t = min(i for i, (x, empty, y) in enumerate(d) if empty and x >= cur[0] and y is None)
            if t >= f:
                continue
        except ValueError:
            # no room to move cur
            continue

        diff = d[t][0] - cur[0]
        if diff == 0:
            d[t], d[f] = d[f], d[t]
        else:
            assert diff > 0
            d[t] = (d[f][0], False, d[f][2])  # used
            d[f] = (d[f][0], True, None)  # clear space at the end
            d.insert(t + 1, (diff, True, None))  # rem empty space

    debug(d)

    return checksum(d)


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        d = parse(fp)
        p1 = solve1(deepcopy(d))
        p2 = solve2(deepcopy(d))

    assert p1 == 1928
    assert p2 == 2858


def main():
    with open(0) as fp:
        d = parse(fp)
        p1 = solve1(deepcopy(d))
        p2 = solve2(deepcopy(d))

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()

####################################################

###### 10
from collections import Counter
from pathlib import Path


def dfs(grid):
    def _inner(s):
        # print('>>> dfs from', s)
        assert grid[s] == 0
        vis = {s}
        st = [s]
        tot = Counter()

        while st:
            cur = st.pop()
            for dz in [-1, 1, -1j, 1j]:
                nxt = cur + dz
                if nxt in grid and grid[nxt] - grid[cur] == 1:
                    if grid[nxt] == 9:
                        tot[nxt] += 1
                    else:
                        vis.add(nxt)
                        st.append(nxt)
        return tot

    res = {}
    for n, h in grid.items():
        if h == 0:
            res[n] = _inner(n)
    return res


def solve(fp):
    grid = {
        complex(j, i): int(x)
        for i, row in enumerate(fp.read().strip().split("\n"))
        for j, x in enumerate(row)
    }

    cnt = dfs(grid)
    p1 = sum(len(dst) for src, dst in cnt.items())
    p2 = sum(sum(dst.values()) for src, dst in cnt.items())

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 36
    assert p2 == 81


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()

#############################################################
