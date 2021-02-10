import sys
import numpy as np
from typing import List, Tuple, Set
from itertools import combinations, product
from collections import defaultdict
from functools import reduce
import operator
import pickle

N, W, S, E = range(4)


@dataclass
class Tile:
    tid: int
    raw: List[str]
    margins: Tuple[int]

    def __hash__(self):
        return self.tid


def read_input(file_path: str) -> List[Tile]:
    with open(file_path, "rt") as fp:
        xs = fp.read().strip().split("\n\n")
        xs = [x.split() for x in xs]
        xs = [(int(x[1].strip(":")), x[2:]) for x in xs]

    return [Tile(tid=tid, raw=raw, margins=encode(raw)) for tid, raw in xs]


def encode(raw: List[str]) -> Tuple[int]:
    """
    (N, W, S, E) encoding of margins.
    [string] -> [bits] -> margins -> to int.
    """
    mat = [list(l.replace(".", "0").replace("#", "1")) for l in raw]
    mat = np.array(mat)

    return tuple(
        map(
            lambda m: int("".join(m), 2),
            [
                mat[0, ::-1],  # n: r->l
                mat[:, 0],  # w: u->d
                mat[-1, :],  # s: l->r
                mat[::-1, -1],  # e: d->u
            ],
        )
    )


def _rbits(x: int, sz=10) -> int:
    return int(bin(x)[2:].zfill(sz)[::-1], 2)


def rot(x: Tuple[int], k=1) -> Tuple[int]:
    once = (x[E], x[N], x[W], x[S])
    return once if k == 1 else rot(once, k - 1)


def flipud(x: Tuple[int]) -> Tuple[int]:
    return tuple(map(_rbits, [x[S], x[W], x[N], x[E]]))


def fliplr(x: Tuple[int]) -> Tuple[int]:
    # return tuple(map(_rbits, [x[N], x[E], x[S], x[W]]))
    return rot(flipud(x), k=2)


def all_transforms(m: Tuple[int]) -> Set[Tuple[int]]:
    # all possible flips/rots
    return {
        "id": m,
        "rot1": rot(m, 1),
        "rot2": rot(m, 2),
        "rot3": rot(m, 3),
        "fliplr": fliplr(m),
        "flipud": flipud(m),
    }


def matches_with(x: Tuple[int], y: Tuple[int]) -> List[int]:
    out = []

    if x[N] == y[S]:
        out.append((N, S))
    if x[E] == y[W]:
        out.append((E, W))
    if x[S] == y[N]:
        out.append((S, N))
    if x[W] == y[E]:
        out.append((W, E))

    return out


def compat_table(tiles: List[Tile]) -> dict:
    """
    tid1 -> {transform -> [N/W/S/E]: tid2}
    """

    def _to_dict(d):
        if isinstance(d, defaultdict):
            d = {k: _to_dict(v) for k, v in d.items()}
        return d

    compat = defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: {})))

    for tile1, tile2 in combinations(tiles, 2):
        tfs1 = all_transforms(tile1.margins).items()
        tfs2 = all_transforms(tile2.margins).items()

        for (i, tf1), (j, tf2) in product(tfs1, tfs2):
            for m1, m2 in matches_with(tf1, tf2):
                compat[tile1.tid][i][m1] = tile2.tid
                compat[tile2.tid][j][m2] = tile1.tid

    return _to_dict(compat)


def is_corner(tfs: set) -> bool:
    rs = reduce(operator.or_, map(lambda r: set(r.values()), tfs.values()))
    return len(rs) == 2


def tile_by_tid(tid: int, tiles: List[Tile]) -> Tile:
    for t in tiles:
        if t.tid == tid:
            return t
    assert False


def next_on(tile: Tile, on: set, compat: dict, exc: set) -> set:
    out = set()

    for t, m in compat[tile.tid].items():
        for o in set(m.keys()) & on:
            out.add((t, m[o]))

    out = {(t, x) for (t, x) in out if x not in exc}
    return out


def is_done(grid):
    for l in grid:
        if None in l:
            return False
    return True


def solve(grid, ori, tiles, compat, exc):
    if is_done(grid):
        return True

    size = len(grid)

    # 2) fill first row (matching e->w with prev)
    for i in range(size):
        if grid[0][i] is not None:
            continue

        ms = next_on(grid[0][i - 1], on={E}, compat=compat, exc=exc)
        xs = {x for _, x in ms}
        d = defaultdict(lambda: [])
        for t, x in ms:
            d[x].append(t)

        for m in xs:
            for dt in d[m]:
                q = tile_by_tid(m, tiles)

                grid[0][i] = q
                ori[0][i] = dt
                exc.add(q.tid)

                if solve(grid, ori, tiles, compat, exc):
                    return True

                grid[0][i] = None
                ori[0][i] = None
                exc.remove(q.tid)
            return False

    # 3) fill first col (with s->n matching)
    for i in range(size):
        if grid[i][0] is not None:
            continue

        ms = next_on(grid[i - 1][0], on={S}, compat=compat, exc=exc)
        xs = {x for _, x in ms}
        d = defaultdict(lambda: [])
        for t, x in ms:
            d[x].append(t)

        for m in xs:
            for dt in d[m]:
                q = tile_by_tid(m, tiles)

                grid[i][0] = q
                ori[i][0] = dt
                exc.add(q.tid)

                if solve(grid, ori, tiles, compat, exc):
                    return True

                grid[i][0] = None
                ori[i][0] = None
                exc.remove(q.tid)
        return False

    # 4) fill rest (with nw matching)
    for i in range(1, size):
        for j in range(1, size):
            if grid[i][j] is not None:
                continue

            d = defaultdict(lambda: [])
            m1 = next_on(grid[i][j - 1], on={E}, compat=compat, exc=exc)
            xs1 = {x for _, x in m1}
            for t, x in m1:
                d[x].append(t)

            m2 = next_on(grid[i - 1][j], on={S}, compat=compat, exc=exc)
            xs2 = {x for _, x in m2}
            for t, x in m2:
                d[x].append(t)

            for m in xs1 & xs2:
                for dt in d[m]:
                    q = tile_by_tid(m, tiles)

                    grid[i][j] = q
                    ori[i][j] = dt
                    exc.add(q.tid)

                    if solve(grid, ori, tiles, compat, exc):
                        return True

                    grid[i][j] = None
                    ori[i][j] = None
                    exc.remove(q.tid)
            return False

    return False


def backtrack(tiles: List[Tile]) -> np.array:
    """
    TODO: can choose any tile as the top-left corner.
    """
    size = np.sqrt(len(tiles)).astype(int)
    grid = [[None for _ in range(size)] for _ in range(size)]
    ori = [[None for _ in range(size)] for _ in range(size)]
    compat = compat_table(tiles)

    # 1) fix to-left corner (one of the 4 corners)
    corners = [tile.tid for tile in tiles if is_corner(compat[tile.tid])]
    # corners = [3433, 3833, 2011, 3001]
    exc = set()

    for c in corners:
        for t, cs in compat[c].items():
            if set(cs.keys()) == {E, S}:
                ori[0][0] = t
                grid[0][0] = tile_by_tid(c, tiles)
                exc.add(c)
                break
        if grid[0][0] is not None:
            break

    assert solve(grid, ori, tiles, compat, exc), "Cannot find solution!"

    for i in range(size):
        for j in range(size):
            print(grid[i][j].tid, end="\t")
        print()

    print("-" * 32)

    for i in ori:
        for j in i:
            print(j, end="\t")
        print()

    with open(f'sol2_{sys.argv[1].split("/")[-1].split(".")[0]}.pkl', "wb") as fp:
        pickle.dump((grid, ori), fp)


def assemble(tiles):
    with open(f'sol2_{sys.argv[1].split("/")[-1].split(".")[0]}.pkl', "rb") as fp:
        grid, ori = pickle.load(fp)

    n = len(grid)

    for i in range(n):
        for j in range(n):
            print(grid[i][j].tid, end="\t")
        print()

    print("-" * 32)

    for i in ori:
        for j in i:
            print(j, end="\t")
        print()

    final = np.chararray(shape=(10 * n, 10 * n))

    for i in range(n):
        for j in range(n):
            tile = np.array([list(x) for x in grid[i][j].raw])

            if ori[i][j] == "id":
                pass
            elif ori[i][j] == "rot1":
                tile = np.rot90(tile, k=1)
            elif ori[i][j] == "rot2":
                tile = np.rot90(tile, k=2)
            elif ori[i][j] == "rot3":
                tile = np.rot90(tile, k=3)
            elif ori[i][j] == "fliplr":
                tile = np.fliplr(tile)
            elif ori[i][j] == "flipud":
                tile = np.flipud(tile)
            else:
                assert False

            final[i * 10 : (i + 1) * 10, j * 10 : (j + 1) * 10] = tile

    for i in range(10 * n):
        for j in range(10 * n):
            print(final[i, j].decode("utf8"), end="")
            if j % 10 == 9:
                print(" ", end="")
        if i % 10 == 9:
            print("\n")
        print()


if __name__ == "__main__":
    tiles = read_input(sys.argv[1])

    backtrack(tiles)
    print("=" * 32)
    assemble(tiles)
