import sys
import re
import numpy as np
from itertools import chain
from typing import List


def parse_constraint(s: str) -> tuple:
    m = re.match(r'([\w ]+): (.+)', s)

    name = m.group(1)
    rs = [x.split('-') for x in m.group(2).split(' or ')]
    rs = [(int(x[0]), int(x[1])) for x in rs]

    return name, rs


def get_valid_fields(val: int, cs: dict) -> set:
    """
    A value is valid if there's at least one field's interval which contains it.
    """
    return {
        field
        for field, intervals in cs.items()
        if any(map(lambda i: i[0] <= val <= i[1], intervals))
    }


def get_invalid_values(ticket: List[int], cs: dict) -> List[int]:
    return [val for val in ticket if 0 == len(get_valid_fields(val, cs))]


def is_valid_ticket(ticket: List[int], cs: dict) -> bool:
    return 0 == len(get_invalid_values(ticket, cs))


def get_ordered_fields(nearby: List[List[int]], cs: dict) -> List[str]:
    def __find_valid_fields(values: List[int], cs: dict) -> set:
        """
        A field is valid (for a list of values)
        if all values respect the contraints for that field.
        """
        vf = set(cs.keys())
        for val in values:
            vf &= get_valid_fields(val, cs)
        return vf

    valid_nearby = np.array([t for t in nearby if is_valid_ticket(t, cs)])

    pool = sorted(
        [(i, __find_valid_fields(vs, cs)) for i, vs in enumerate(valid_nearby.T)],
        key=lambda q: len(q[1]))

    return backtrack(pool)


def backtrack(pool):
    def _inner(pool, acc):
        if None not in acc:
            return True

        for i, ps in pool:
            for p in ps - set(acc):
                acc[i] = p
                if _inner(pool[1:], acc):
                    return True
                acc[i] = None

        return False

    acc = [None] * len(pool)
    _inner(pool, acc)
    return acc


if __name__ == "__main__":
    with open(sys.argv[1], 'rt') as fp:
        cs, my, nearby = fp.read().strip().split('\n\n')

    # ticket = {field: value}

    cs = dict(map(parse_constraint, cs.split('\n')))
    my: List[int] = list(map(int, my.split('\n')[1].split(',')))

    nearby: List[List[int]] = [
        list(map(int, t.split(','))) for t in nearby.split('\n')[1:]
    ]

    invalid_values = [get_invalid_values(t, cs) for t in nearby]
    print(f'Part 1: {sum(chain(*invalid_values))}')

    # backtracking guided by sorting candidates by size
    ordered_fields = get_ordered_fields(nearby, cs)
    dep_fields = [i for i, k in enumerate(ordered_fields) if k.startswith('departure')]
    print(f'Part 2: {np.product([my[i] for i in dep_fields])}')