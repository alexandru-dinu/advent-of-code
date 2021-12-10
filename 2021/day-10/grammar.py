from __future__ import annotations

import sys

from lark import Lark, UnexpectedCharacters, UnexpectedEOF

from common import corrupt_score, incomplete_score

grammar = """
    start: s

    s: round s | square s | brace s | angle s |
    round:  "(" s ")"
    square: "[" s "]"
    brace:  "{" s "}"
    angle:  "<" s ">"
"""


def correct_string(s: str, parser) -> str:
    """
    Assuming `s` is an incomplete string, find the needed closing characters.
    """
    i = len(s)

    while True:
        try:
            parser.parse(s)
            break  # no errors

        except UnexpectedEOF as exc:
            # e.expected is the list of expected chars
            # since we only care about closing chars,
            # find "R"(PAR,SQB,BRACE) or "MORETHAN"
            if "MORETHAN" in exc.expected:
                s += ">"
            elif "RPAR" in exc.expected:
                s += ")"
            elif "RSQB" in exc.expected:
                s += "]"
            elif "RBRACE" in exc.expected:
                s += "}"
            else:
                assert False, "unreachable"

    return s[i:]


def solve(xs: list[str]) -> tuple[int, int]:
    parser = Lark(grammar)
    cor = 0
    inc = []

    for x in xs:
        try:
            parser.parse(x)

        except UnexpectedCharacters as exc:
            # exc.char is the unexpected character,
            # e.g. ] in ( ... ] ; expected a ')'
            cor += corrupt_score(exc.char)

        except UnexpectedEOF:
            inc.append(incomplete_score(correct_string(x, parser)))

    return cor, sorted(inc)[len(inc) // 2]


def main():
    with open(sys.argv[1]) as fp:
        xs = [l.strip() for l in fp.readlines()]

    corrupt, incomplete = solve(xs)

    print("Part 1:", corrupt)
    print("Part 2:", incomplete)


if __name__ == "__main__":
    main()
