def batchify(xs, batch_size):
    n = len(xs)
    for i in range(0, n, batch_size):
        yield xs[i : min(i + batch_size, n)]


def item_priority(x):
    return 1 + (ord(x) - ord("A")) % 32 + 26 * x.isupper()


def set_priority(xs):
    return item_priority(set.intersection(*map(set, xs)).pop())


def main():
    with open(0) as fp:
        xss = [x.strip() for x in fp]

    print("Part 1:", sum(map(lambda xs: set_priority(batchify(xs, len(xs) // 2)), xss)))
    print("Part 2:", sum(map(set_priority, batchify(xss, 3))))


if __name__ == "__main__":
    main()
