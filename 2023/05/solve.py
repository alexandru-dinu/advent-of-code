import re
from collections import defaultdict
from typing import TextIO

from tqdm import tqdm


def parse(fp):
    inp = iter(fp.read().strip().split("\n\n"))

    seeds = [int(x) for x in re.findall(r"\d+", next(inp))]

    maps = defaultdict(dict)

    for group in inp:
        header, *ms = group.split("\n")
        src, dst = re.search(r"(\w+)-to-(\w+)", header).groups()
        for m in ms:
            i, j, d = map(int, m.split())
            maps[src][range(j, j + d)] = range(i, i + d)

        # maps[src].sort(key=lambda r: r.start)

    return seeds, dict(maps)


def move(seed, maps):
    x = seed

    for key, ms in maps.items():
        for src, dst in ms.items():
            if x in src:
                x = dst[x - src.start]
                break
        # implicit: any unmapped src numbers correspond to the same dst number

    return x


def solve(fp: TextIO) -> tuple[int, int]:
    seeds, maps = parse(fp)

    # seeds are actual values
    p1 = min(map(lambda s: move(s, maps), seeds))

    # seeds are ranges (start, len), ...
    assert len(seeds) % 2 == 0
    seeds = [range(i, i + d) for i, d in zip(*[iter(seeds)] * 2)]

    t = float("inf")
    for rng in tqdm(seeds):
        for s in tqdm(rng):
            t = min(t, move(s, maps))

    p2 = t
    return p1, p2


def test_example() -> None:
    with open("example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 35
    assert p2 == 46


def main() -> None:
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
