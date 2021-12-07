import sys

import numpy as np


def gauss(n):
    return 0.5 * n * (n + 1)


def binary_search(pos, score_func):
    xs = np.arange(0, pos.max())
    n = len(xs)
    val = np.infty

    lo, hi = 0, n - 1

    while lo <= hi:
        mid = lo + (hi - lo) // 2

        sl = None if mid == 0 else score_func(pos, xs[mid - 1])
        sr = None if mid == n - 1 else score_func(pos, xs[mid + 1])
        sv = score_func(pos, xs[mid])

        val = min(sv, val)

        if sl < sv:
            hi = mid - 1
        elif sr < sv:
            lo = mid + 1
        else:
            break

    return val


def solve_binary_search(pos):
    """
    Use binary search to find the min. value of the score function.
    """
    print("Using binary search")

    score_func = lambda xs, x: np.abs(xs - x).sum()
    print("Part 1:", binary_search(pos, score_func))

    score_func = lambda xs, x: gauss(np.abs(xs - x)).sum()
    print("Part 1:", binary_search(pos, score_func))


def solve_direct(pos):
    """
    Solve using direct calculation -> median & mean.
    See https://math.stackexchange.com/a/1024462
    """
    print("Using direct calculation")

    x = np.median(pos)
    score = np.abs(pos - x).sum()
    print("Part 1:", int(score))

    # lo <= x <= hi, x int
    lo = np.mean(pos) - 1 / 2
    hi = np.mean(pos) + 1 / 2

    score = min(
        gauss(np.abs(pos - x)).sum() for x in np.arange(np.floor(lo), np.ceil(hi) + 1)
    )
    print("Part 2:", int(score))


def main():
    pos = np.loadtxt(sys.argv[1], delimiter=",", dtype=int)

    solve_binary_search(pos)
    solve_direct(pos)


if __name__ == "__main__":
    main()
