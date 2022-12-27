from __future__ import annotations

import json
from argparse import ArgumentParser
from dataclasses import dataclass
from functools import reduce
from itertools import product
from math import ceil, floor
from typing import Union


@dataclass
class Regular:
    val: int

    def __str__(self) -> str:
        return str(self.val)

    @property
    def adt(self) -> str:
        return f"{self.__class__.__name__} {self.val}"


@dataclass
class Pair:
    """Binary tree representing the expression pair."""

    l: Regular | Pair | None = None
    r: Regular | Pair | None = None

    def __str__(self) -> str:
        return f"[{str(self.l)},{str(self.r)}]"

    @property
    def adt(self) -> str:
        return f"{self.__class__.__name__} ({self.l.adt}) ({self.r.adt})"


Expr = Union[Regular, Pair]


def make_expr(xs: list) -> Expr:
    a, b = xs

    expr = Pair()

    expr.l = make_expr(a) if isinstance(a, list) else Regular(a)
    expr.r = make_expr(b) if isinstance(b, list) else Regular(b)

    return expr


def add_on(expr: Expr, val: int, on: str) -> Expr:
    if val == 0:
        return expr

    if isinstance(expr, Regular):
        return Regular(expr.val + val)

    if on == "left":
        return Pair(l=add_on(expr.l, val, on), r=expr.r)

    if on == "right":
        return Pair(l=expr.l, r=add_on(expr.r, val, on))


def split(expr: Expr) -> tuple[bool, Expr]:
    """(`did` flag, sub-tree)"""
    if isinstance(expr, Regular):
        if expr.val >= 10:
            l = Regular(floor(expr.val / 2))
            r = Regular(ceil(expr.val / 2))
            return (True, Pair(l, r))
        else:
            return (False, expr)

    assert isinstance(expr, Pair)

    did, lexpr = split(expr.l)
    if did:
        return (did, Pair(l=lexpr, r=expr.r))

    did, rexpr = split(expr.r)
    if did:
        return (did, Pair(l=expr.l, r=rexpr))

    return (False, expr)


def explode(expr: Pair, level: int = 0) -> tuple[bool, Pair, tuple[int] | None]:
    """(`did` flag, sub-tree, exploded node l/r values)"""
    if isinstance(expr, Regular):
        return (False, expr, (0, 0))

    if isinstance(expr.l, Regular) and isinstance(expr.r, Regular):
        if level >= 4:
            return (True, Regular(0), (expr.l.val, expr.r.val))
        else:
            return (False, expr, (0, 0))

    did, lexpr, (x, y) = explode(expr.l, level + 1)
    if did:
        return (did, Pair(l=lexpr, r=add_on(expr.r, y, on="left")), (x, 0))

    did, rexpr, (x, y) = explode(expr.r, level + 1)
    if did:
        return (did, Pair(l=add_on(expr.l, x, on="right"), r=rexpr), (0, y))

    return (False, expr, (0, 0))


def simplify(expr: Pair) -> Pair:
    did, expr2, *_ = explode(expr)
    if did:
        return simplify(expr2)

    did, expr2 = split(expr)
    if did:
        return simplify(expr2)

    return expr


def magnitude(expr: Pair) -> int:
    if isinstance(expr, Regular):
        return expr.val

    return 3 * magnitude(expr.l) + 2 * magnitude(expr.r)


def part1(exprs: [Pair]) -> int:
    return magnitude(reduce(lambda acc, expr: simplify(Pair(acc, expr)), exprs))


def part2(exprs: [Pair]) -> int:
    mag = -1
    for (i, j) in product(range(len(exprs)), repeat=2):
        if i != j:
            mag = max(mag, magnitude(simplify(Pair(exprs[i], exprs[j]))))

    return mag


def main():
    with open(args.file) as fp:
        exprs = [make_expr(json.loads(x)) for x in fp.read().strip().split("\n")]

    if args.adt:
        for e in exprs:
            print(e.adt)
        return

    print("Part 1:", part1(exprs))
    print("Part 2:", part2(exprs))


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("--file", type=str, required=True)
    parser.add_argument(
        "--adt", action="store_true", help="If set, then just parse the input into ADT."
    )
    args = parser.parse_args()
    main()
