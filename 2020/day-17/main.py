import sys
from itertools import *


def flatmap(f, xs):
    return list(chain(*map(f, xs)))


def iterate(f, x0):
    return accumulate(repeat(x0), lambda l, _: f(l))


def take(xs, n):
    return islice(xs, n)


def zip_with(f, xs, ys):
    return tuple(map(f, xs, ys))


def neighbours(cell) -> set:
    dim = len(cell)
    ns = [
        zip_with(lambda x, y: x + y, cell, delta)
        for delta in product([-1, 0, 1], repeat=dim)
        if delta != (0,) * dim
    ]
    return set(ns)


def count_active(cell, pocket) -> int:
    return len(neighbours(cell) & pocket)


def will_be_active(cell, pocket):
    na = count_active(cell, pocket)
    return (na in [2, 3]) if cell in pocket else (na == 3)


def step(pocket):
    expanded = flatmap(lambda cell: neighbours(cell), pocket)
    active = filter(lambda cell: will_be_active(cell, pocket), expanded)
    return set(active)


def get_pocket(filename, dims=3):
    pocket = set()
    with open(sys.argv[1], "rt") as fp:
        for y, line in enumerate(fp.read().strip().split("\n")):
            pocket |= set(
                [(x, y) + (0,) * (dims - 2) for x, c in enumerate(line) if c == "#"]
            )
    return pocket


if __name__ == "__main__":
    num_iter = int(sys.argv[2])

    *_, final = take(iterate(step, get_pocket(sys.argv[1], dims=3)), num_iter + 1)
    print(f"Part 1: {len(final)}")

    *_, final = take(iterate(step, get_pocket(sys.argv[1], dims=4)), num_iter + 1)
    print(f"Part 1: {len(final)}")
