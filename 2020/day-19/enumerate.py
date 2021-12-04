import re
import sys
from itertools import *


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


def parse_input(s: str) -> tuple:
    rules, msgs = s.split("\n\n")

    msg_set = set(msgs.split("\n"))
    rule_dict = {}

    for i, rs in [r.split(": ") for r in rules.split("\n")]:
        if m := re.match(r'"([a-z]+)"', rs):
            rule_dict[int(i)] = m.group(1)
        else:
            rs = [tuple(map(int, r.split())) for r in rs.split(" | ")]
            rule_dict[int(i)] = rs

    return rule_dict, msg_set


if __name__ == "__main__":
    with open(sys.argv[1], "rt") as fp:
        rule_dict, msg_set = parse_input(fp.read().strip())

    r0 = enumerate(rule=0, rule_dict=rule_dict)
    assert len(r0) == 1  # rule 0 has only one exact match

    print(f"Part 1: {len(set(r0[0]) & msg_set)}")
