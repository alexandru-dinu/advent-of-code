from collections import defaultdict
from pathlib import Path
from typing import TextIO

from tqdm import tqdm


def parse(fp: TextIO):
    grid = {
        complex(i, j): x for i, row in enumerate(fp.read().split("\n")) for j, x in enumerate(row)
    }
    src = [k for k, v in grid.items() if v == "S"]
    assert len(src) == 1
    return grid, src[0]


def recompose_path(parents, src, ori):
    path = [(src, ori)]

    while (nxt := parents[path[-1]]) is not None:
        path.append(nxt)

    path = path[::-1]

    cost = 0
    for (zn, dn), (zp, dp) in zip(path[1:], path):
        assert abs(zn - zp) == 1
        cost += 1 + (dn != dp) * 1000

    return cost, path


def dfs(grid, src):
    ori = 1j
    st = [(src, ori)]

    cost = defaultdict(lambda: float("inf"))
    cost[(src, ori)] = 0

    parents = defaultdict(lambda: None)
    parents[(src, ori)] = None

    best_cost = float("inf")
    pbar = tqdm(desc="DFS")

    while st:
        cur, ori = st.pop()
        cur_cost = cost[(cur, ori)]

        if grid.get(cur) == "E":
            best_cost = min(best_cost, cur_cost)
            _cost, _path = recompose_path(parents, cur, ori)
            assert _cost == cur_cost

            pbar.update(1)
            pbar.set_description(f"{best_cost=}")

            yield _cost, _path

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
                parents[(nxt, new_ori)] = (cur, ori)


def solve(grid, src):
    paths = sorted(dfs(grid, src), key=lambda kv: kv[0])
    best_cost = paths[0][0]

    p1 = best_cost
    p2 = len(set(sum(([p[0] for p in ps] for c, ps in paths if c == best_cost), [])))

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example1") as fp:
        p1, p2 = solve(*parse(fp))
        assert 7036 == p1
        assert 45 == p2

    with open(Path(__file__).parent / "example2") as fp:
        p1, p2 = solve(*parse(fp))
        assert 11048 == p1
        assert 64 == p2


def main():
    with open(0) as fp:
        p1, p2 = solve(*parse(fp))

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
