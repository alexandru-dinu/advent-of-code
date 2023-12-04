import numpy as np


def view_gen(xss: np.ndarray):
    h, w = xss.shape

    for i in range(1, h - 1):
        for j in range(1, w - 1):
            u = xss[i, j] > xss[:i, j][::-1]
            d = xss[i, j] > xss[i + 1 :, j]
            l = xss[i, j] > xss[i, :j][::-1]
            r = xss[i, j] > xss[i, j + 1 :]

            yield u, d, l, r


def count_visible(mask):
    return mask.sum() if mask.all() else 1 + mask.argmin()


def main():
    with open(0) as fp:
        xss = np.array([[int(x) for x in xs.strip()] for xs in fp])

    part1 = 2 * np.sum(xss.shape) - 4  # start from the border
    part2 = -1

    for view in view_gen(xss):
        part1 += any(map(all, view))
        part2 = max(part2, np.prod([count_visible(mask) for mask in view]))

    print("Part 1:", part1)
    print("Part 2:", part2)


if __name__ == "__main__":
    main()
