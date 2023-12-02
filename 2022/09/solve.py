from copy import deepcopy

import matplotlib.pyplot as plt
import numpy as np
from vec2 import Vec2

UP = Vec2(0, 1)
DOWN = Vec2(0, -1)
LEFT = Vec2(-1, 0)
RIGHT = Vec2(1, 0)


def simulate(oris, cnts, num_knots) -> list[Vec2]:
    """Rope: [head, *knots]"""
    rope = [Vec2(0, 0)] + [Vec2(0, 0) for _ in range(num_knots)]
    trace = [deepcopy(rope)]

    for ori, cnt in zip(oris, cnts):
        for _ in range(cnt):
            # update position of head
            if ori == "U":
                rope[0] += UP
            elif ori == "D":
                rope[0] += DOWN
            elif ori == "L":
                rope[0] += LEFT
            elif ori == "R":
                rope[0] += RIGHT

            # update position of knots
            for i in range(1, len(rope)):
                delta = rope[i - 1] - rope[i]

                dx = [LEFT, Vec2(0, 0), RIGHT][np.sign(delta.x) + 1]
                dy = [DOWN, Vec2(0, 0), UP][np.sign(delta.y) + 1]

                if abs(delta.x) > 1 or abs(delta.y) > 1:
                    rope[i] += dx + dy

            trace.append(deepcopy(rope))

    return trace


def visualize_plot(trace, markersize=10):
    head, *tails = zip(*trace)

    plt.figure()

    hx, hy = zip(*head)
    plt.plot(hx, hy, "r--", marker="o", markersize=markersize, label="head")

    for i, tail in enumerate(tails, start=1):
        tx, ty = zip(*tail)
        plt.plot(tx, ty, ls="--", marker="o", markersize=markersize, label=f"tail {i}")

    plt.grid(True)
    plt.legend()
    plt.show()


def main():
    with open(0) as fp:
        oris, cnts = zip(*[x.strip().split() for x in fp])
        cnts = list(map(int, cnts))

    # H, K1, K2, ..., (K9 = T)
    trace = simulate(oris, cnts, num_knots=9)
    # visualize_plot(trace, markersize=2)

    print("Part 1:", len(set([t[1] for t in trace])))
    print("Part 1:", len(set([t[9] for t in trace])))


if __name__ == "__main__":
    main()
