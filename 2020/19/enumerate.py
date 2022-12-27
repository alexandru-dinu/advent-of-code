import sys
from itertools import *

from common import parse_input


def combine(*args):
    return ["".join(tup) for tup in product(*args)]


def enumerate(rule, rule_dict) -> list:
    def _inner(rule, memo: dict) -> list:
        if not isinstance(rule, list) and rule in memo:
            return memo[rule]

        if isinstance(rule, int):
            y = rule_dict[rule]
            if isinstance(y, str):  # leaf
                return [y]
            else:
                q = _inner(y, memo)
                memo[rule] = q
                return q

        if isinstance(rule, tuple):
            qs = [_inner(x, memo) for x in rule]
            qs = combine(*[chain(*q) for q in qs])
            memo[rule] = qs
            return qs

        assert isinstance(rule, list)
        return [_inner(x, memo) for x in rule]

    return _inner(rule, memo={})


if __name__ == "__main__":
    with open(sys.argv[1], "rt") as fp:
        rule_dict, msg_set = parse_input(fp.read().strip())

    r0 = enumerate(rule=0, rule_dict=rule_dict)
    assert len(r0) == 1  # rule 0 has only one exact match

    print(f"Part 1: {len(set(r0[0]) & msg_set)}")
