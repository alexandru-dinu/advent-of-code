import re
from collections import Counter
from typing import TextIO

from toolz.dicttoolz import keyfilter


def part1(x: str) -> bool:
    for s in ["ab", "cd", "pq", "xy"]:
        if s in x:
            return False

    if not sum(keyfilter(lambda k: k in "aeiou", Counter(x)).values()) >= 3:
        return False

    if not any(x[i] == x[i - 1] for i in range(1, len(x))):
        return False

    return True


def part2(x: str) -> bool:
    for i in range(len(x) - 1):
        m = re.finditer(x[i : i + 2], x)
        try:
            next(m), next(m)
            break
        except StopIteration:
            continue
    else:
        return False

    return re.search(r"([a-z])[a-z]\1", x) is not None


def solve(fp: TextIO) -> tuple[int, int]:
    xs = fp.read().strip().split("\n")

    p1 = sum(part1(x) for x in xs)
    p2 = sum(part2(x) for x in xs)

    return p1, p2


def test_example() -> None:
    assert part1("ugknbfddgicrmopn")
    assert part1("aaa")
    assert not part1("jchzalrnumimnmhp")
    assert not part1("haegwjzuvuyypxyu")
    assert not part1("dvszwmarrgswjxmb")

    assert part2("qjhvhtzxzqqjkmpb")
    assert part2("xxyxx")
    assert not part2("uurcxstgmygtbstg")
    assert not part2("ieodomkazucvgmuy")


def main() -> None:
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
