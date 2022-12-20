DEC_KEY = 811589153


def dec(xs, num=1):
    n = len(xs)
    xs = list(enumerate(xs))

    for orig_i, x in xs * num:
        i = xs.index((orig_i, x))
        j = (i + x) % (n - 1)  # -1 because of the pop
        xs.insert(j, xs.pop(i))

    _, xs = zip(*xs)
    i0 = xs.index(0)

    return sum(xs[(i0 + o) % n] for o in (1000, 2000, 3000))


def main():
    with open(0) as fp:
        xs = [int(x.strip()) for x in fp]

    print("Part 1:", dec(xs, num=1))
    print("Part 2:", dec([x * DEC_KEY for x in xs], num=10))


if __name__ == "__main__":
    main()
