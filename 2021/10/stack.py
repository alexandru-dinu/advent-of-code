from __future__ import annotations

import sys
from collections import namedtuple
from enum import Enum, auto

from common import corrupt_score, incomplete_score


class Status(Enum):
    SUCCESS = auto()
    CORRUPT = auto()
    INCOMPLETE = auto()


Info = namedtuple("Info", "exp found col stack")

closing = {"(": ")", "[": "]", "{": "}", "<": ">"}


def try_parse(xs: str) -> tuple[Status, Info | None]:
    stack = []

    for i, x in enumerate(xs):
        if x in closing:
            stack.append(x)
        else:
            top = stack.pop()
            exp = closing[top]
            if x != closing[top]:
                return Status.CORRUPT, Info(exp, x, i, stack)

    if len(stack) > 0:
        exp = stack[-1]
        return Status.INCOMPLETE, Info(exp, None, len(xs), stack)

    return Status.SUCCESS, None


def solve(xs: list[str]):
    cor = 0
    inc = []

    for x in xs:
        status, info = try_parse(x)

        if status == Status.CORRUPT:
            cor += corrupt_score(info.found)

        elif status == Status.INCOMPLETE:
            # get necessary closing characters from the stack
            rem = "".join(closing[c] for c in info.stack[::-1])
            inc.append(incomplete_score(rem))

        else:
            assert status == Status.SUCCESS

    return cor, sorted(inc)[len(inc) // 2]


def main():
    with open(sys.argv[1]) as fp:
        xs = [l.strip() for l in fp.readlines()]

    corrupt, incomplete = solve(xs)

    print("Part 1:", corrupt)
    print("Part 2:", incomplete)


if __name__ == "__main__":
    main()
