from __future__ import annotations

import sys

import numpy as np


class PairSim:
    def __init__(self, i: int, j: int, ins_mat: np.ndarray):
        n = len(ins_mat)
        self.ins_mat = ins_mat

        self.elem_cnt = np.zeros(n, dtype=int)
        self.elem_cnt[i] += 1
        self.elem_cnt[j] += 1

        self.freq_mat = np.zeros((n, n), dtype=int)
        self.freq_mat[i, j] = 1

    def step(self, n: int = 1) -> PairSim:
        for _ in range(n):
            fm = self.freq_mat.copy()
            self.freq_mat.fill(0)

            for i, j in zip(*np.where(fm > 0)):
                # k is produced between (i, j) => ik, kj
                k = self.ins_mat[i, j]

                self.freq_mat[i, k] += fm[i, j]
                self.freq_mat[k, j] += fm[i, j]

                self.elem_cnt[k] += fm[i, j]

        return self


def to_numeric(pattern, rules) -> tuple:
    """Convert elements to numbers 0..n as it's easier to work with."""
    elems = sorted(set("".join(rules.keys())))
    e2i = {e: i for i, e in enumerate(elems)}
    n = len(elems)

    xs = [e2i[e] for e in pattern]

    ins_mat = np.zeros((n, n), dtype=int)
    for (u, v), w in rules.items():
        i, j, k = map(e2i.get, (u, v, w))
        ins_mat[i, j] = k

    return xs, ins_mat


def get_diff_count(xs, ins_mat, n):
    """
    Simulate `n` steps then return the diff between
    the most and least common elements.
    """
    cnt = np.zeros(len(ins_mat), dtype=int)

    for i, j in zip(xs[:-1], xs[1:]):
        cnt += PairSim(i, j, ins_mat).step(n).elem_cnt

    # account for double counting middle elements
    for c in xs[1:-1]:
        cnt[c] -= 1

    return cnt.max() - cnt.min()


def main():
    with open(sys.argv[1]) as fp:
        pattern, rules = fp.read().strip().split("\n\n")
        rules = dict(r.split(" -> ") for r in rules.split("\n"))

    xs, ins_mat = to_numeric(pattern, rules)

    print("Part 1:", get_diff_count(xs, ins_mat, n=10))
    print("Part 2:", get_diff_count(xs, ins_mat, n=40))


if __name__ == "__main__":
    main()
