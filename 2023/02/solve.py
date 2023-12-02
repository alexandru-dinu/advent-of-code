import math
import re
from collections import defaultdict

from toolz import dicttoolz


def parse_line(line: str) -> dict:
    draws = defaultdict(list)
    pattern = re.compile(r"(\d+) (red|green|blue){1}")

    for n, color in pattern.findall(line.split(":")[1]):
        draws[color].append(int(n))

    return draws


def part1(draws: dict) -> int:
    constr = {"red": 12, "green": 13, "blue": 14}

    def cond(draw):
        return all(max(draw[c]) <= thr for c, thr in constr.items())

    return sum(gid * cond(draw) for gid, draw in enumerate(draws, start=1))


def part2(draws: dict) -> int:
    return sum(math.prod(dicttoolz.valmap(max, draw).values()) for draw in draws)


def test_example():
    from pathlib import Path

    with open(Path(__file__).parent / "example") as fp:
        draws = [parse_line(x.strip()) for x in fp]

    assert part1(draws) == 8
    assert part2(draws) == 2286


def main():
    with open(0) as fp:
        draws = [parse_line(x.strip()) for x in fp]

    print(f"Part 1: {part1(draws)}")
    print(f"Part 2: {part2(draws)}")


if __name__ == "__main__":
    main()
