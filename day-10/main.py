import sys


def get_difference_product(zs: set):
    xs = zs.copy()
    d1 = d3 = 0
    src = 0

    while len(xs) > 0:
        # if there are multiple possibilities, greedily select min
        adapter = min(xs & {src + 1, src + 2, src + 3})

        xs.remove(adapter)
        diff = adapter - src
        d1  += (diff == 1)
        d3  += (diff == 3)
        src  = adapter

    # + 1 for own device which always has a diff of 3
    return d1 * (d3 + 1)


def enumerate_combinations(xs: set):
    def _inner(zs: set, current: int, target: int, acc: set, cs: list):
        if current == target:
            cs.append(acc.copy())
            return

        for a in zs & {current + 1, current + 2, current + 3}:
            zs.remove(a)
            acc.add(a)

            _inner(zs, a, target, acc, cs)

            acc.remove(a)
            zs.add(a)

    cs = []
    zs = xs.copy()
    target = max(zs) + 3
    zs.add(target)
    _inner(zs, current=0, target=target, acc=set(), cs=cs)

    return cs


def count_combinations(xs: set) -> int:
    """
               0, 1, 2, 3, 4, 5, ...
                     [--j--]  i
     e.g. xs: [0, 1, 4, 5, 6, 7, ...]
    num_ways: [1, 1, 1, 1, 2, 4, ...]

    there are 4 pos. of reaching from 4 to 7:
    1) 4 (+1) 5 (+1) 6 (+1) 7
    2) 4 (+1) 5 (+2) 7
    3) 4 (+2) 6 (+1) 7
    4) 4 (+3) 7

    thus: num_ways[5] = num_ways[4] + num_ways[3] + num_ways[2]

    NOTE: looks like a (masked) Tribonacci(len(xs))
    also similar to Fibonacci staircase (where you can climb 1 or 2 stairs at once)
    here you can "move" 3 steps at once, so just count the number of ways to get to those steps:
    4 -> 7 (+3)
    5 -> 7 (+2)
    6 -> 7 (+1)
    """
    zs = [0, *sorted(xs), max(xs) + 3]

    # num_ways[i] = # of combinations up to size i
    num_ways = [0] * len(zs)
    num_ways[0] = 1

    for i in range(1, len(zs)):
        # look back a window s.t. diff <= 3
        # add the number of ways to get there
        # explicitly verbose
        num_ways[i] = num_ways[i-1] * (zs[i] - zs[i-1] <= 3) + \
                      num_ways[i-2] * (zs[i] - zs[i-2] <= 3) + \
                      num_ways[i-3] * (zs[i] - zs[i-3] <= 3)

    return num_ways[-1]


if __name__ == "__main__":
    with open(sys.argv[1], 'rt') as fp:
        xs = set([int(x.strip()) for x in fp.readlines()])

    print(f'Part 1: {get_difference_product(xs)}')
    print(f'Part 2: {count_combinations(xs)}')

    # print(enumerate_combinations(xs))
