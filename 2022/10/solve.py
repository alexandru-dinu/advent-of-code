from itertools import takewhile

import numpy as np

ADDX_CYCLES = 2


def execute_gen(lines):
    clk, x = 1, 1
    yield (clk, x)

    for op, *val in map(lambda x: x.split(), lines):
        if op == "noop":
            clk += 1

        elif op == "addx":
            for _ in range(ADDX_CYCLES - 1):
                clk += 1
                yield (clk, x)

            x += int(val.pop())
            clk += 1

        yield (clk, x)


def part1(lines):
    cycles = np.arange(20, 221, 40)
    signal_strength = 0

    out = execute_gen(lines)

    for cyc in cycles:
        z = list(takewhile(lambda x: x[0] <= cyc, out))
        signal_strength += np.product(z.pop())

    return signal_strength


def part1_alt(lines):
    def parse_input(lines):
        xs = [1]  # initial value of X

        for op, *val in map(lambda x: x.split(), lines):
            if op == "noop":
                xs.append(0)
            elif op == "addx":
                xs.extend([0, int(val.pop())])

        return np.array(xs)

    cycles = np.arange(20, 221, 40)
    xs = np.cumsum(parse_input(lines))
    return np.dot(cycles, xs[cycles - 1])


def part2(lines):
    h, w = 6, 40
    clk, xs = map(np.array, zip(*execute_gen(lines)))
    assert len(xs) == h * w + 1

    crt = np.full((h, w), " ")

    for i, x in enumerate(xs):
        if i % w in [x - 1, x, x + 1]:
            crt[i // w, i % w] = "#"

    return "\n".join(map(lambda row: "".join(row), crt))


def main():
    with open(0) as fp:
        lines = [x.strip() for x in fp]

    print("Part 1:", part1(lines))
    print("Part 1 (alt):", part1_alt(lines))

    print(f"Part 2:\n{part2(lines)}")


if __name__ == "__main__":
    main()
