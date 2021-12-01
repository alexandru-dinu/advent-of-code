import sys

import numpy as np


def part1(xs):
    return np.sum(np.diff(xs) > 0)


def conv_valid(xs, w):
    """Computes the 'valid' convolution between `xs` and `w`, as given by

    np.convolve(xs, w, mode='valid')
    """
    n = len(xs)
    m = len(w)

    idx = np.expand_dims(np.arange(n - m + 1), 1) + np.arange(m)

    return np.sum(xs[idx] * w[::-1], axis=1)


def part2(xs, win_size=3):
    return part1(conv_valid(xs, np.ones(win_size)))


def main():
    with open(sys.argv[1]) as fp:
        xs = np.array([int(x.strip()) for x in fp.readlines()])

    print(f"Part 1: {part1(xs)}")
    print(f"Part 2: {part2(xs)}")


if __name__ == "__main__":
    main()
