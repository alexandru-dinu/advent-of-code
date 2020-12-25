import sys
import numpy as np
from typing import List, Tuple, Set
from functools import partial
from itertools import combinations, product
from collections import deque, namedtuple, defaultdict


Tile = namedtuple('Tile', ['tid', 'border'])
N, W, S, E = range(4)


def encode(raw: str) -> Tuple[int]:
    """
    (N, W, S, E) encoding of border.
    Raw tile string -> bit array -> get border -> to int
    """
    lines = raw.replace('.', '0').replace('#', '1').split('\n')
    mat = np.array([list(line) for line in lines])

    return tuple(map(lambda m: int(''.join(m), 2), [
        mat[0, ::-1],  # n: r->l
        mat[:, 0],     # w: u->d
        mat[-1, :],    # s: l->r
        mat[::-1, -1], # e: d->u
    ]))


def rbits(x: int, sz=10) -> int:
    return int(bin(x)[2:].zfill(sz)[::-1], 2)

def flipud(x: Tuple[int]) -> Tuple[int]:
    return tuple(map(rbits, [x[S], x[W], x[N], x[E]]))

def fliplr(x: Tuple[int]) -> Tuple[int]:
    return tuple(map(rbits, [x[N], x[E], x[S], x[W]]))

def rot(x: Tuple[int], k=1) -> Tuple[int]:
    once = (x[E], x[N], x[W], x[S])
    return once if k == 1 else rot(once, k - 1)


def all_transforms(m: Tuple[int]) -> Set[Tuple[int]]:
    # all possible flips/rots
    return {m, fliplr(m), flipud(m), *[rot(m, k) for k in range(1, 4)]}


def matches_with(x: Tuple[int], y: Tuple[int]) -> List[int]:
    out = []

    if x[E] == y[W]:
        out.append(E)

    if x[N] == y[S]:
        out.append(N)

    if x[S] == y[N]:
        out.append(S)

    if x[W] == y[E]:
        out.append(W)

    return out


def compat_table(tiles: List[Tile]) -> dict:
    compat = defaultdict(lambda: defaultdict(lambda: set()))

    for tile1, tile2 in combinations(tiles, 2):
        xs = all_transforms(tile1.border)
        ys = all_transforms(tile2.border)
        for x, y in product(xs, ys):
            for m in matches_with(x, y):
                compat[tile1.tid][tile2.tid].add(m)
                compat[tile2.tid][tile1.tid].add(m)

    return compat


def corner_ids(tiles: List[Tile]) -> List[int]:
    compat = compat_table(tiles)
    cids = [tid for tid, cs in compat.items() if len(cs) == 2]
    assert len(cids) == 4, f"Expected to find 4 corners, but got {len(cids)}"
    return cids


if __name__ == "__main__":
    with open(sys.argv[1], "rt") as fp:
        tiles = fp.read().strip().split("\n\n")

    tiles = [t.split(":\n") for t in tiles]
    tiles = [Tile(tid=int(i.split()[1]), border=encode(t)) for i, t in tiles]
    # print([(i, t.tid) for i,t in enumerate(tiles)])

    # while True:
    #     i = int(input())
    #     is_corner_tile(tiles[i], tiles)
    #     print('-' * 32)

    print(f"Part 1: {np.product(corner_ids(tiles))}")