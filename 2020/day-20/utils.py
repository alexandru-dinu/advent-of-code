import numpy as np
from typing import List, Tuple
from dataclasses import dataclass

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
