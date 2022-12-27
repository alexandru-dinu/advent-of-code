def search(xs, size):
    n = len(xs) - (size - 1)

    for i in range(n):
        if len(set(xs[i : i + size])) == size:
            return i + size

    assert False, "unreachable"


def main():
    with open(0) as fp:
        xs = fp.read().strip()

    print("Part 1:", search(xs, size=4))
    print("Part 2:", search(xs, size=14))


if __name__ == "__main__":
    main()
