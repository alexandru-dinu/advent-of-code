import numpy as np


def maxmin_diff(xs):
    return max(xs) - min(xs)


def div_diff(xs):
    ys = sorted(set(xs), reverse=True)
    n = len(xs)

    for i in range(n):
        for j in range(i + 1, n):
            d, m = divmod(ys[i], ys[j])
            if m == 0:
                return d

    assert False, "unreachable"


def main():
    with open(0) as fp:
        xss = [np.fromstring(x, dtype=int, sep="\t") for x in fp.read().splitlines()]

    print("Part 1:", sum(map(maxmin_diff, xss)))
    print("Part 2:", sum(map(div_diff, xss)))


if __name__ == "__main__":
    main()
