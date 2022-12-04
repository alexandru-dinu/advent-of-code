from itertools import cycle

import numpy as np


def main():
    with open(0) as fp:
        xs = np.array([int(x.strip()) for x in fp])

    print("Part 1:", xs.sum())

    f = 0
    seen = {f}
    for i in cycle(range(len(xs))):
        f += xs[i]
        if f in seen:
            print("Part 2:", f)
            break
        seen.add(f)


if __name__ == "__main__":
    main()
