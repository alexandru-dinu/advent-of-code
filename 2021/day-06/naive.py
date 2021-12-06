from argparse import ArgumentParser

import matplotlib.pyplot as plt
import numpy as np
from sklearn.linear_model import LinearRegression


def advance(xs):
    mask = xs == 0

    if (n := mask.sum()) > 0:
        xs[mask] = 6
        xs[~mask] -= 1
        xs = np.concatenate((xs, 8 * np.ones(n)))
    else:
        xs[~mask] -= 1

    return xs


def repl(xs):
    while True:
        n = int(input("Advance > ").strip())
        for _ in range(n):
            xs = advance(xs)
        print(xs)
        print(f"{len(xs)=}")
        print("-" * 64)


def main(xs):
    ds = [0]
    cs = [len(xs)]

    for d in range(1, args.days + 1):
        xs = advance(xs)
        ds.append(d)
        cs.append(len(xs))

    print(f"Part 1: {cs[-1]:d}")

    ds, cs = map(np.array, (ds, cs))

    model = LinearRegression(fit_intercept=False)
    X = np.array(ds).reshape(-1, 1)
    y = np.log2(cs / cs[0])
    model.fit(X, y)

    """
    logy = mx + b
    y = 2**b * (2**m)**x = a * (2**(1/tau))**x
    => tau = 1/m => freq of doubling
    """

    m = model.coef_.item()
    b = np.log2(cs[0])  # model.intercept_

    tau = 1 / m
    pred = lambda t: (2 ** b) * (2 ** (t / tau))

    print(f"Init pop: {int(2**b)}")
    print(f"Learned freq of doubling: {tau:.5f}")
    print(f"{pred(80)  = :.5f}")
    print(f"{pred(256) = :.5f}")

    if args.plot:
        plt.scatter(ds, cs, s=10, label="true")
        plt.plot(ds, pred(ds), c="r", label="pred")
        plt.legend()
        plt.xlabel("Number of days", fontsize=14)
        plt.ylabel("Number of fish", fontsize=14)
        plt.title(f"x(t) = {int(2**b)} $\\times$ 2^(t / {tau:.3f})")
        # plt.yscale("log")
        plt.tight_layout()
        plt.show()


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument(
        "--file",
        type=str,
        required=True,
        help="Input file containing initial fish population.",
    )
    parser.add_argument(
        "--days", type=int, required=True, help="Number of days to simulate."
    )
    parser.add_argument(
        "--repl",
        action="store_true",
        help="Observe the fish after advancing `n` steps.",
    )
    parser.add_argument("--plot", action="store_true", help="Plot model.")
    args = parser.parse_args()

    with open(args.file) as fp:
        xs = np.array(
            [int(x.strip()) for x in fp.read().strip().split(",")], dtype=np.uint8
        )

    if args.repl:
        repl(xs)
    else:
        main(xs)
