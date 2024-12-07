import multiprocessing as mp
import os
from itertools import product
from pathlib import Path
from random import shuffle
from typing import TextIO

from tqdm import tqdm


def is_ok(eq, y, ops):
    ops = list(product(ops, repeat=len(eq) - 1))
    shuffle(ops)

    for op in ops:
        n = eq[0]
        for i, j in zip(eq[1:], op):
            n = eval(f"{n}{j}{i}")
        if n == y:
            return True, y

    return False, y


def run(eqs, ops):
    with mp.Pool(os.cpu_count()) as pool:
        res = [pool.apply_async(is_ok, args=(eq, y, ops)) for (eq, y) in eqs]
        res = [r.get() for r in tqdm(res)]
        res = sum(y for x, y in res if x)

    return res


def solve(fp: TextIO):
    eqs = []
    for line in fp.readlines():
        y, eq = line.strip().split(":")
        y = int(y)
        eq = list(map(int, eq.strip().split()))
        eqs.append((eq, y))

    p1 = run(eqs, ops=("+", "*"))
    p2 = run(eqs, ops=("+", "*", ""))

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 3749
    assert p2 == 11387


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
