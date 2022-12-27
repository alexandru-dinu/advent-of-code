from __future__ import annotations

from argparse import ArgumentParser
from itertools import count

import numpy as np
import numpy.ma as ma


def step(xss: np.ndarray) -> np.ndarray:
    # mask stores the cells that have flashed
    # set fill value to 0 to indicate that all cells that flashed have a value of 0
    mat = ma.masked_array(data=xss.copy(), mask=np.zeros_like(xss), fill_value=0)
    h, w = mat.shape

    # the energy level of each oct. increases by 1
    mat += 1

    while True:
        # update each nei. for all cells with energy level > 9
        fi, fj = flash_idx = ma.where(mat > 9)

        # no levels > 9
        if fi.size == fj.size == 0:
            break

        mat.mask[flash_idx] = True

        for i, j in zip(fi, fj):
            i_slice = slice(max(i - 1, 0), min(i + 2, h), 1)
            j_slice = slice(max(j - 1, 0), min(j + 2, w), 1)

            mat[i_slice, j_slice] += 1

    # all cells that flashed (i.e. mask == True) are filled with 0
    return mat.filled()


def count_flashes(xss: np.ndarray, n: int) -> int:
    """Simulate `n` steps and return the total number of flashes."""
    flashes = 0

    for _ in range(n):
        xss = step(xss)
        flashes += np.sum(xss == 0)

    return flashes


def first_sync_flash(xss: np.ndarray) -> int:
    """Return the first step during which all octopuses flash."""
    for i in count(1):
        xss = step(xss)
        if np.all(xss == 0):
            return i


def animate(xss: np.ndarray) -> None:
    import matplotlib.pyplot as plt
    from matplotlib.animation import FuncAnimation
    from matplotlib.colors import ListedColormap

    def init():
        im.set_data(xss)
        return [im]

    def animate(i):
        a = im.get_array()
        im.set_array(step(a))
        return [im]

    fig = plt.figure(facecolor="#0f0f23")
    ax = fig.add_subplot(111)
    ax.axis("off")

    cmap = np.vstack([[0, i / 255, 0, 1] for i in np.linspace(0x33, 0xCC, 9)])
    cmap = np.vstack(([[0x99 / 255, 0, 0, 1]], cmap))

    im = plt.imshow(xss, vmin=0, vmax=9, cmap=ListedColormap(cmap))

    sync = first_sync_flash(xss)

    ani = FuncAnimation(fig, animate, init_func=init, frames=sync + 10)
    ani.save("./ani.gif", writer="imagemagick", fps=2)


def main():
    xss = np.stack(
        [np.fromiter(x, dtype=int) for x in np.loadtxt(args.file, dtype=str)]
    )

    if args.animate:
        animate(xss)
        return

    print("Part 1:", count_flashes(xss, n=100))
    print("Part 2:", first_sync_flash(xss))


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("--file", type=str, required=True)
    parser.add_argument("--animate", action="store_true")
    args = parser.parse_args()

    main()
