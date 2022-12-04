import numpy as np


def offset_sum(xs, offset=1):
    i1 = np.arange(len(xs))
    i2 = (i1 + offset) % len(xs)

    return xs[np.where(xs[i1] == xs[i2])].sum()


def main():
    with open(0) as fp:
        xs = np.array([int(x) for x in fp.read().strip()])

    print("Part 1:", offset_sum(xs, offset=1))
    print("Part 2:", offset_sum(xs, offset=len(xs) // 2))


if __name__ == "__main__":
    main()
