from copy import deepcopy
from dataclasses import dataclass

from tqdm import trange

WIDTH = 7


@dataclass
class Shape:
    kind: int
    zs: set[complex]

    def lim(self, func, attr):
        return func(getattr(z, attr) for z in self.zs)

    def move(self, dz: complex) -> "Shape":
        if dz.real + self.lim(min, "real") < 0:
            return self
        if dz.real + self.lim(max, "real") >= WIDTH:
            return self

        self.zs = {z + dz for z in self.zs}
        return self


SHAPES = [
    # 1/ ####
    Shape(kind=1, zs={0 + 0j, 1 + 0j, 2 + 0j, 3 + 0j}),
    # 2/ .#.
    #    ###
    #    .#.
    Shape(kind=2, zs={1 + 0j, 0 + 1j, 1 + 1j, 2 + 1j, 1 + 2j}),
    # 3/ ..#
    #    ..#
    #    ###
    Shape(kind=3, zs={0 + 0j, 1 + 0j, 2 + 0j, 2 + 1j, 2 + 2j}),
    # 4/ #
    #    #
    #    #
    #    #
    Shape(kind=4, zs={0 + 0j, 0 + 1j, 0 + 2j, 0 + 3j}),
    # 5/ ##
    #    ##
    Shape(kind=5, zs={0 + 0j, 1 + 0j, 0 + 1j, 1 + 1j}),
]


def simulate(wind: list[str], num_iter: int, use_cache=False) -> list[int]:
    occ = {x + 0j for x in range(WIDTH)}
    tops = [0]
    mem = {}
    s_idx = w_idx = 0

    for t in trange(num_iter, ascii=True):
        if use_cache:
            if (s_idx, w_idx) in mem:
                # ~~~~~~~~~|---------| ... |---------|
                #    ^     ^         ^
                #  extra s(t)     s(t+tau): tau = period
                # s(t) = s(t+tau)
                # h(t) = h(t + k*tau) + k*dH
                # after some iterations we have a cycles of length tau
                t_, top_ = mem[(s_idx, w_idx)]
                tau = t - t_
                dH = tops[-1] - top_
                # try to compute the result for all steps
                d, m = divmod(num_iter - t_, tau)
                if m == 0:
                    return t, int(top_ + d * dH)
            else:
                mem[(s_idx, w_idx)] = (t, tops[-1])

        # spawn a new shape
        shape = deepcopy(SHAPES[s_idx]).move(2 + (tops[-1] + 4) * 1j)
        s_idx = (s_idx + 1) % len(SHAPES)
        assert occ & shape.zs == set()

        while not (occ & shape.zs):
            w = wind[w_idx]
            w_idx = (w_idx + 1) % len(wind)
            if w == ">":
                shape.move(1 + 0j)
                if shape.zs & occ:
                    shape.move(-1 + 0j)
            elif w == "<":
                shape.move(-1 + 0j)
                if shape.zs & occ:
                    shape.move(1 + 0j)
            else:
                assert False

            shape.move(0 - 1j)

        # collision: update occ and continue with the next shape
        shape.move(0 + 1j)  # final position
        occ |= shape.zs
        tops.append(int(max(z.imag for z in occ)))

    return tops


def main():
    with open(0) as fp:
        wind = fp.read().strip()

    print("Part 1:", simulate(wind, num_iter=2022, use_cache=False)[-1])
    print("Part 2:", simulate(wind, num_iter=int(1e12), use_cache=True))

    # import numpy as np
    # tops = simulate(wind, num_iter=20_000, use_cache=False)
    # np.save('tops_input_20k.npy', tops)


if __name__ == "__main__":
    main()
