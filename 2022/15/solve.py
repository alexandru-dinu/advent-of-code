import math
import multiprocessing as mp
import os
import re
from functools import partial

import matplotlib.pyplot as plt
import z3
from tqdm import tqdm

d = r"(-?\d+)"
pattern = re.compile(f"Sensor at x={d}, y={d}: closest beacon is at x={d}, y={d}")


def parse_line(line):
    sx, sy, bx, by = map(int, pattern.match(line).groups())
    return (sx + sy * 1j), (bx + by * 1j)


def rot45(xs):
    a = math.pi / 4
    r = math.cos(a) + math.sin(a) * 1j
    return [x * r for x in xs]


def visualise(sensors, beacons):
    plt.figure(dpi=200)
    plt.scatter(*zip(*[(s.real, s.imag) for s in sensors]))
    plt.scatter(*zip(*[(b.real, b.imag) for b in beacons]), c="red")
    ax = plt.gca()
    ax.set_ylim(ax.get_ylim()[::-1])
    plt.grid(True)
    plt.show()


def manh(x, y):
    return abs(x.real - y.real) + abs(x.imag - y.imag)


def work(data, y_offset, rng):
    count = 0
    lo, hi = rng

    for x in range(lo, hi):
        cur = x + y_offset * 1j
        for s, b in data:
            if cur == b or cur == s:
                break

            # if cur is within the range of the sensor
            if manh(cur, s) <= manh(s, b):
                count += 1
                break

    return count


def part1(sensors, beacons, y_offset):
    # sort by distance to y_offset
    data = sorted(zip(sensors, beacons), key=lambda sb: abs(y_offset - sb[0].imag))

    s2b = {s: b for s, b in data}
    b2s = {b: s for s, b in data}

    # leftmost
    lo_s = min(sensors, key=lambda s: s.real)
    lo_b = min(beacons, key=lambda b: b.real)
    if lo_b.real < lo_s.real:  # beacon is leftmost
        lo = lo_b.real - manh(lo_b, b2s[lo_b])
    else:  # sensor is leftmost
        lo = lo_s.real - manh(lo_s, s2b[lo_s])

    # rightmost
    hi_s = max(sensors, key=lambda s: s.real)
    hi_b = max(beacons, key=lambda b: b.real)
    if hi_b.real > hi_s.real:  # beacon is rightmost
        hi = hi_b.real + manh(hi_b, b2s[hi_b])
    else:  # sensor is rightmost
        hi = hi_s.real + manh(hi_s, s2b[hi_s])

    # split range from lo to hi in batches of size 10000
    batch_size = 1000
    range_ = list(range(int(lo), int(hi) + 1, batch_size))
    batches = [*zip(range_, range_[1:]), (range_[-1], int(hi) + 1)]

    work_fn = partial(work, data, y_offset)
    count = 0

    with mp.Pool(processes=os.cpu_count()) as pool:
        with tqdm(total=len(batches), ascii=True) as pbar:
            for cnt in pool.imap_unordered(work_fn, batches):
                count += cnt
                pbar.update(1)

    return count


def part2(sensors, beacons):
    lo = 0
    hi = int(4e6)

    solver = z3.Solver()
    # define the search space
    x, y = z3.Ints("x y")
    solver.add(lo <= x, x <= hi, lo <= y, y <= hi)
    # there's only one beacon not found by any sensor
    # meaning that (x, y) is not part of any sensor's range
    for s, b in zip(sensors, beacons):
        # sensor's range is given by d(*, s) <= d(s, b)
        d_sb = z3.Abs(s.real - b.real) + z3.Abs(s.imag - b.imag)
        d_cs = z3.Abs(x - s.real) + z3.Abs(y - s.imag)
        solver.add(z3.Not(d_cs <= d_sb))

    assert solver.check() == z3.sat

    model = solver.model()

    x = model[x].as_long()
    y = model[y].as_long()

    return x * hi + y


def main():
    with open(0) as fp:
        sensors, beacons = zip(*[parse_line(x.strip()) for x in fp])

    # visualise(sensors, beacons)
    # visualise(preprocess(sensors), preprocess(beacons))

    print("Part 1:", part1(sensors, beacons, y_offset=2_000_000))
    print("Part 2:", part2(sensors, beacons))


if __name__ == "__main__":
    main()
