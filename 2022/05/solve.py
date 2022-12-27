import re
from collections import defaultdict
from copy import deepcopy


def parse_stacks(raw: str) -> dict:
    stacks = defaultdict(list)

    idx, *crates = raw.split("\n")[::-1]
    idx = list(map(int, idx.split()))

    for row in crates:
        for i, c in zip(idx, row[1::4]):
            if c.isspace():
                continue
            stacks[i].append(c)

    return dict(stacks)


def parse_insts(raw: str) -> list:
    out = []

    for line in raw.split("\n"):
        cnt, beg, end = map(
            int, re.match(r"move (\d+) from (\d+) to (\d+)", line).groups()
        )
        out.append((cnt, beg, end))

    return out


def move(stacks, insts, reverse):
    for cnt, beg, end in insts:
        take = [stacks[beg].pop() for _ in range(cnt)]
        if reverse:
            take = take[::-1]
        stacks[end].extend(take)

    # concat top crates
    return "".join(s[-1] for s in stacks.values())


def main():
    with open(0) as fp:
        raw_stacks, raw_insts = fp.read().split("\n\n")

    insts = parse_insts(raw_insts)
    stacks = parse_stacks(raw_stacks)

    print("Part 1:", move(deepcopy(stacks), insts, reverse=False))
    print("Part 2:", move(deepcopy(stacks), insts, reverse=True))


if __name__ == "__main__":
    main()
