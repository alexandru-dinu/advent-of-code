from __future__ import annotations

import sys

import numpy as np
import numpy.ma as ma


def iterate(drawn: list[int], boards: ma.core.MaskedArray, need_to_win: int):
    _, _, board_size = boards.shape

    idx_won = []

    for num in drawn:
        # mark current number
        boards.mask[boards.data == num] = True

        # check if there is a board with a fully marked row/col
        win = np.argwhere(
            np.logical_or(
                boards.mask.sum(axis=1) == board_size,
                boards.mask.sum(axis=2) == board_size,
            )
        )

        # get the index of the completed boards
        idx = np.setdiff1d(win[:, 0], idx_won)
        if idx.size == 0:
            continue

        idx_won.extend(idx)

        if len(idx_won) == need_to_win:
            # sum of all unmarked numbers is given w.r.t. mask
            return num * boards[idx_won[-1]].sum()


def main():
    with open(sys.argv[1]) as fp:
        drawn = [int(x) for x in fp.readline().strip().split(",")]
        boards = np.loadtxt(fp, dtype=int).reshape(-1, 5, 5)
        boards = ma.masked_array(boards, mask=np.zeros_like(boards))

    print("Part 1:", iterate(drawn, boards.copy(), need_to_win=1))
    print("Part 2:", iterate(drawn, boards.copy(), need_to_win=len(boards)))


if __name__ == "__main__":
    main()
