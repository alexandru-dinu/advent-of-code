from itertools import cycle


def part1(xs):
    return sum(xs)


def part2(xs):
    f = 0
    seen = {f}

    for i in cycle(range(len(xs))):
        f += xs[i]
        if f in seen:
            return f
        seen.add(f)


def test_example():
    assert part1([1, 1, 1]) == 3
    assert part1([1, 1, -2]) == 0
    assert part1([-1, -2, -3]) == -6

    assert part2([+1, -1]) == 0
    assert part2([+3, +3, +4, -2, -4]) == 10
    assert part2([-6, +3, +8, +5, -6]) == 5
    assert part2([+7, +7, -2, -7, -4]) == 14


def main():
    with open(0) as fp:
        xs = [int(x.strip()) for x in fp]

    print(f"Part 1: {part1(xs)}")
    print(f"Part 2: {part2(xs)}")


if __name__ == "__main__":
    main()
