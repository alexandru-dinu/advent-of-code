import sys

import numpy as np


def convolve(mat):
    h, w = mat.shape
    aug = np.pad(mat, ((1, 1), (1, 1)), mode="constant", constant_value=0)


def part1(enh, xs):
    import IPython

    IPython.embed(using=False)


def main():
    with open(sys.argv[1]) as fp:
        contents = fp.read().strip()
        contents = contents.replace("#", "1").replace(".", "0")
        enh, xs = contents.split("\n\n")
        enh = np.fromiter(enh, dtype=int)
        xs = np.stack([np.fromiter(x, dtype=int) for x in xs.split()])

    part1(enh, xs)


if __name__ == "__main__":
    main()
