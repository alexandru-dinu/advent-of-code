import sys
import re
from collections import namedtuple


regex = re.compile(r"(\d+)(-)(\d+)( )([a-z])(: )([a-z]+)")
Entry = namedtuple("Entry", ["lo", "hi", "char", "password"])


def parse(x: str) -> Entry:
    gs = regex.match(x).groups()
    return Entry(lo=int(gs[0]), hi=int(gs[2]), char=gs[4], password=gs[6])


def is_valid1(x: Entry) -> bool:
    return x.lo <= x.password.count(x.char) <= x.hi


def is_valid2(x: Entry) -> bool:
    l, h = x.lo - 1, x.hi - 1
    return (x.password[l] == x.char) ^ (x.password[h] == x.char)


if __name__ == "__main__":
    with open(sys.argv[1], "rt") as fp:
        xs = [parse(l.strip()) for l in fp.readlines()]

    ans1 = sum(map(is_valid1, xs))
    ans2 = sum(map(is_valid2, xs))

    print(f"Part 1: {ans1}")
    print(f"Part 2: {ans2}")
