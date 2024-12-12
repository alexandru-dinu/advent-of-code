from copy import deepcopy
from pathlib import Path


def parse(fp):
    # [(amount, blockID)]
    disk = []
    bid = 0

    for i, amt in enumerate(fp.read().strip()):
        free = i % 2 == 1
        disk.append((int(amt), bid if not free else None))
        if free:
            bid += 1

    return disk


def checksum(d):
    tot = 0
    i = 0

    for amt, bid in d:
        if bid is not None:
            for j in range(amt):
                tot += i * bid
                i += 1
        else:
            i += amt

    return tot


def solve1(d):
    # [t]rue = empty
    # [f]alse = used

    # t = 1
    # f = len(d) - 1 - (len(d) % 2 == 0)

    # TODO: replace w/ proper inc/dec
    t = min(i for i, (_, bid) in enumerate(d) if bid is None)
    f = max(i for i, (_, bid) in enumerate(d) if not bid is None)

    while t < f and 0 <= t < len(d) and 0 <= f < len(d):
        print(f - t)
        if d[t][0] == d[f][0]:
            d[t], d[f] = d[f], d[t]

        elif d[t][0] > d[f][0]:
            diff = d[t][0] - d[f][0]
            d[t] = (d[f][0], d[f][1])  # used
            d[f] = (d[f][0], None)  # clear space at the end
            d.insert(t + 1, (diff, None))  # rem empty space

        else:
            diff = d[f][0] - d[t][0]
            d[t] = (d[t][0], d[f][1])
            d[f] = (diff, d[f][1])
            d.insert(f + 1, (d[t][0], None))

        t = min(i for i, (_, bid) in enumerate(d) if bid is None)
        f = max(i for i, (_, bid) in enumerate(d) if not bid is None)

    # all used space then all free space
    for i, (_, bid) in enumerate(d):
        if bid is None:
            for j in range(i, len(d)):
                assert d[j][1] is None

    return checksum(d)


def solve2(d):
    f = max(i for i, (x, bid) in enumerate(d) if bid is not None)

    for to_move in sorted({bid for (_, bid) in d if bid is not None}, reverse=True):
        print(to_move)
        cur = [f for f, (x, y) in enumerate(d) if y == to_move]
        assert len(cur) == 1
        f = cur[0]
        cur = d[f]

        try:
            t = min(i for i, (amt, bid) in enumerate(d) if bid is None and amt >= cur[0])
            if t >= f:
                continue
        except ValueError:
            # no room to move cur
            continue

        diff = d[t][0] - cur[0]
        if diff == 0:
            d[t], d[f] = d[f], d[t]
        else:
            assert diff > 0
            d[t] = (d[f][0], d[f][1])  # used
            d[f] = (d[f][0], None)  # clear space at the end
            d.insert(t + 1, (diff, None))  # rem empty space

    return checksum(d)


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        d = parse(fp)
        p1 = solve1(deepcopy(d))
        p2 = solve2(deepcopy(d))

    assert p1 == 1928
    assert p2 == 2858


def main():
    with open(0) as fp:
        d = parse(fp)
        p1 = solve1(deepcopy(d))
        p2 = solve2(deepcopy(d))

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
