import sys

def solve(xs: set, src: int):
    d1 = d3 = 0

    while len(xs) > 0:
        # if there are multiple possibilities, greedily select min
        adapter = min(xs & {src + 1, src + 2, src + 3})

        xs.remove(adapter)
        diff = adapter - src
        d1  += (diff == 1)
        d3  += (diff == 3)
        src  = adapter

    return d1 * (d3 + 1)


if __name__ == "__main__":
    with open(sys.argv[1], 'rt') as fp:
        xs = set([int(x.strip()) for x in fp.readlines()])

    print(f'Part 1: {solve(xs, src=0)}')