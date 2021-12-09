from __future__ import annotations

from argparse import ArgumentParser
from collections import deque

import numpy as np


def count_lte(mat: np.ndarray) -> np.ndarray:
    """
    lte[i,j] = count (neighbours <= mat[i,j])
    . t .
    l . r
    . b .
    """
    aug = np.pad(mat.astype(float), (1, 1), mode="constant", constant_values=np.inf)

    l = aug[1:-1, :-2] <= mat
    r = aug[1:-1, 2:] <= mat
    t = aug[:-2, 1:-1] <= mat
    b = aug[2:, 1:-1] <= mat

    return l + r + t + b


def part1(xs):
    lte = count_lte(xs)
    return np.sum(1 + xs[lte == 0])


def get_basin(xs: np.ndarray, row: int, col: int) -> list[tuple[int, int]]:
    """
    Return the indices of the locations flowing towards the low point `row, col`.
    """
    h, w = xs.shape
    out = []

    q = deque()
    v = np.zeros_like(xs).astype(bool)

    q.append((row, col))
    v[row, col] = True

    while q:
        i, j = q.popleft()
        out.append((i, j))

        for di, dj in [(0, -1), (0, 1), (-1, 0), (1, 0)]:
            i2 = i + di
            j2 = j + dj

            if not (0 <= i2 < h) or not (0 <= j2 < w):
                continue

            if v[i2, j2]:
                continue

            if xs[i2, j2] == 9:
                continue

            q.append((i2, j2))
            v[i2, j2] = True

    return out


def part2(xs):
    lte = count_lte(xs)

    basins = [get_basin(xs, row, col) for row, col in zip(*np.where(lte == 0))]

    top = sorted(map(len, basins), reverse=True)

    return np.product(top[:3])


def visualize(xs):
    import matplotlib.pyplot as plt
    from matplotlib import cm
    from matplotlib.colors import ListedColormap

    lte = count_lte(xs)

    cmap = cm.Blues_r(np.linspace(0, 1, 10))
    cmap[-1] = [0, 0, 0, 1]

    plt.imshow(xs, cmap=ListedColormap(cmap))

    basins = sorted(
        [get_basin(xs, row, col) for row, col in zip(*np.where(lte == 0))],
        key=len,
        reverse=True,
    )

    cmap = cm.viridis(np.linspace(0.8, 0.2, 6))
    for i in range(3):
        r, c = zip(*basins[i])
        plt.scatter(c, r, c=[cmap[i * 2]], marker="s")

    r, c = np.where(lte == 0)
    plt.scatter(c, r, c="red", marker="x")

    plt.show()


def main():
    with open(args.file) as fp:
        xs = np.array([[int(i) for i in x.strip()] for x in fp.readlines()])

    if args.visualize:
        visualize(xs)
        return

    print("Part 1:", part1(xs))
    print("Part 2:", part2(xs))


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("--file", type=str, required=True)
    parser.add_argument(
        "--visualize",
        action="store_true",
        help="Visualize the map with low points and basins",
    )
    args = parser.parse_args()

    main()
