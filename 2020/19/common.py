import re


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
