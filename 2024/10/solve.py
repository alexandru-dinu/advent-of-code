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
