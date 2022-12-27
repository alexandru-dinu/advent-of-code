import heapq
import sys


def partition(xs, l, r):
    """Canonical QuickSort partition."""
    pivot = xs[r]
    idx = l

    for i in range(l, r):
        if xs[i] <= pivot:
            xs[i], xs[idx] = xs[idx], xs[i]
            idx += 1

    xs[idx], xs[r] = xs[r], xs[idx]

    return idx


def topk(xs, k):
    n = len(xs)
    l = 0
    r = n - 1

    while l < r:
        idx = partition(xs, l, r)

        if idx == n - k:
            break

        if idx > n - k:
            r = idx - 1
        else:
            l = idx + 1

    return xs[-k:]


def solve1(xs, k=3):
    """Default sort"""
    ys = sorted(xs, reverse=True)

    p1 = ys[0]
    p2 = sum(ys[:k])

    return p1, p2


def solve2(xs, k=3):
    """Partial sort"""
    ys = topk(xs, k)

    p1 = max(ys)
    p2 = sum(ys)

    return p1, p2


def solve3(xs, k=3):
    """Using a heap."""
    k = min(k, len(xs))
    zs = [-x for x in xs]

    heapq.heapify(zs)
    ys = [-heapq.heappop(zs) for _ in range(k)]
    p1 = max(ys)
    p2 = sum(ys)

    return p1, p2


def get_input(f: str) -> list[int]:
    with open(f) as fp:
        xss = fp.read().strip().split("\n\n")

    sum_of = lambda xs: int(sum(map(float, xs.split())))

    return list(map(sum_of, xss))


def main():
    xs = get_input(sys.argv[1])

    solvers = [func for func in globals().keys() if func.startswith("solve")]

    for solver in solvers:
        p1, p2 = globals().get(solver)(xs)
        print(solver)
        print("\tPart 1:", p1)
        print("\tPart 2:", p2)


if __name__ == "__main__":
    main()
