import re
import sys

from lark import Lark


def get_grammar(s: str) -> str:
    return re.sub(r"(\d+)", r"r\1", s).replace("r0:", "start:")


def try_parse(p, s) -> bool:
    try:
        p.parse(s)
        return True
    except:
        return False


if __name__ == "__main__":
    with open(sys.argv[1], "rt") as fp:
        g, m = fp.read().strip().split("\n\n")

    m = m.split("\n")

    p = Lark(get_grammar(g))
    print(f"Part 1: {sum([try_parse(p, s) for s in m])}")

    g = g.replace("8: 42", "8: 42 | 42 8")
    g = g.replace("11: 42 31", "11: 42 31 | 42 11 31")
    p = Lark(get_grammar(g))
    print(f"Part 2: {sum([try_parse(p, s) for s in m])}")
