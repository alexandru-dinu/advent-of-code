import time
from collections import defaultdict
from itertools import count

import numpy as np

# NW, N, NE, W, E, SW, S, SE
OFFSETS = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]


def grow(mat):
    # grow matrix so that we don't have to check for out-of-bounds
    t = int(mat[0, :].max())
    b = int(mat[-1, :].max())
    l = int(mat[:, 0].max())
    r = int(mat[:, -1].max())
    return np.pad(mat, ((t, b), (l, r)), "constant", constant_values=0)


def step_round(mat, k):
    moved = False
    mat = grow(mat)
    proposals = defaultdict(list)

    # 1/ collect proposals
    for i, j in zip(*np.where(mat == 1)):
        # if no other occupied pos, the elf does not move
        if 0 == sum(map(mat.__getitem__, [(i + y, j + x) for y, x in OFFSETS])):
            continue

        deltas = [
            ((i - 1, j), not any(mat[i - 1, j + o] for o in (-1, 0, 1))),  # N, NE, NW
            ((i + 1, j), not any(mat[i + 1, j + o] for o in (-1, 0, 1))),  # S, SE, SW
            ((i, j - 1), not any(mat[i + o, j - 1] for o in (-1, 0, 1))),  # W, NW, SW
            ((i, j + 1), not any(mat[i + o, j + 1] for o in (-1, 0, 1))),  # E, NE, SE
        ]
        for q in range(4):
            nxt, cond = deltas[(k + q) % 4]
            if cond:
                proposals[nxt].append((i, j))
                break
        else:
            continue  # no proposal

    # 2/ apply proposals
    # the elf moves to its proposal if and only if it is the only elf who proposed it
    for (i, j), elf_pos in proposals.items():
        if len(elf_pos) == 1:
            mat[i, j] = 1
            mat[elf_pos.pop()] = 0
            moved = True

    return mat, moved


def part1(mat):
    for k in range(10):
        mat, _ = step_round(mat, k % 4)

    # get sub-matrix containing all elves
    ys, xs = np.where(mat == 1)
    bb = mat[ys.min() : ys.max() + 1, xs.min() : xs.max() + 1]

    return (bb == 0).sum()


def part2(mat):
    for k in count(0):
        t0 = time.perf_counter_ns()
        mat, moved = step_round(mat, k % 4)
        t1 = time.perf_counter_ns()
        # print(f"Round {k + 1} took {(t1 - t0) / 1e6:.2f} ms", end="\r")
        if not moved:
            # print()
            return k + 1


def show_mat(mat):
    f = lambda i: ".#"[int(i)]
    print("\n".join(map(lambda x: "".join(map(f, x)), mat)))


def main():
    with open(0) as fp:
        mat = np.array([[x == "#" for x in xs.strip()] for xs in fp], dtype=bool)

    print("Part 1:", part1(mat.copy()))
    print("Part 2:", part2(mat.copy()))


if __name__ == "__main__":
    main()
