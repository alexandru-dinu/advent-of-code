from collections import defaultdict
from pathlib import Path
from typing import TextIO


def recompose_path(parent, src):
    path = [src]
    cur = src

    while (nxt := parent[src]) is not None:
        path.append(nxt)
        src = nxt

    return path[::-1]


def parse(fp: TextIO):
    grid = {
        complex(i, j): x for i, row in enumerate(fp.read().split("\n")) for j, x in enumerate(row)
    }
    src = [k for k, v in grid.items() if v == "S"]
    assert len(src) == 1
    return grid, src[0]


def dfs(grid, src):
    ori = 1j
    st = [(src, ori)]

    cost = defaultdict(lambda: float("inf"))
    cost[(src, ori)] = 0

    res = float("inf")

    while st:
        cur, ori = st.pop()
        cur_cost = cost[(cur, ori)]

        if grid.get(cur) == "E":
            print(">>> FOUND", cur_cost)
            res = min(res, cur_cost)
            # print('\tPATH:', recompose_path(parent, cur))
            # print(cost)
            continue

        for new_ori in [ori, ori * 1j, ori * (-1j)]:
            nxt = cur + new_ori

            if nxt not in grid or grid[nxt] == "#":
                continue
            assert grid[nxt] in ".SE"

            new_cost = cur_cost + 1 + (new_ori != ori) * 1000
            if new_cost <= cost[(nxt, new_ori)]:
                cost[(nxt, new_ori)] = new_cost
                st.append((nxt, new_ori))

    return res


def solve(grid, src):
    p1 = dfs(grid, src)
    p2 = ...

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example1") as fp:
        assert 7036 == dfs(*parse(fp))

    with open(Path(__file__).parent / "example2") as fp:
        assert 11048 == dfs(*parse(fp))


def main():
    with open(0) as fp:
        p1, p2 = solve(*parse(fp))

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
