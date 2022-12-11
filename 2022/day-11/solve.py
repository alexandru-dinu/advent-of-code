import re
from collections import deque
from copy import deepcopy
from dataclasses import dataclass
from math import lcm

import numpy as np
from tqdm import trange


@dataclass
class Monkey:
    items: deque[int]
    op: callable
    test: int
    throws: tuple[int, int]  # (if_false, if_true)
    inspected: int = 0


def parse_single(s: str) -> Monkey:
    xs = [x.strip() for x in s.split("\n")]
    assert len(xs) == 6

    r = re.match(r"Monkey (\d+):", xs[0]).group(1)
    idx = int(r)

    r = re.match(r"Starting items: (.+)", xs[1]).group(1)
    items = list(map(int, r.split(", ")))

    r = re.match(r"Operation: new = (.+)", xs[2]).group(1)
    op = eval(f"lambda x: {r.replace('old', 'x')}")

    r = re.match(r"Test: divisible by (\d+)", xs[3]).group(1)
    test = int(r)

    r = re.match(r"If true: throw to monkey (\d+)", xs[4]).group(1)
    if_true = int(r)

    r = re.match(r"If false: throw to monkey (\d+)", xs[5]).group(1)
    if_false = int(r)

    return idx, Monkey(deque(items), op, test, (if_false, if_true))


def simulate(monkeys, num_rounds, div3, verbose=False):
    modulo = lcm(*[m.test for m in monkeys.values()])

    for r in trange(num_rounds, ascii=True):

        for i in monkeys.keys():
            m = monkeys[i]
            m.inspected += len(m.items)

            while m.items:
                w = m.op(m.items.popleft())
                w = (w // 3) if div3 else (w % modulo)
                j = m.throws[w % m.test == 0]
                monkeys[j].items.append(w)

                if verbose:
                    print(f"Monkey {i} throws {w} to monkey {j}")

    return {i: m.inspected for i, m in monkeys.items()}


def top2(xs):
    return np.product(sorted(xs.values(), reverse=True)[:2])


def main():
    with open(0) as fp:
        xs = [x for x in fp.read().split("\n\n")]

    monkeys = {i: x for i, x in map(parse_single, xs)}

    xs = simulate(deepcopy(monkeys), num_rounds=20, div3=True)
    print("Part 1:", top2(xs))

    xs = simulate(deepcopy(monkeys), num_rounds=10000, div3=False)
    print("Part 2:", top2(xs))


if __name__ == "__main__":
    main()
