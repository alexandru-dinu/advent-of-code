import sys
from functools import reduce
from collections import defaultdict
from itertools import chain

# https://www.redblobgames.com/grids/hexagons/
POS = {
    #      dx  dy  dz
    'ne': (+1,  0, -1),
    'e' : (+1, -1,  0),
    'se': ( 0, -1, +1),
    'sw': (-1,  0, +1),
    'w' : (-1, +1,  0),
    'nw': ( 0, +1, -1)
}

WHITE = 0
BLACK = 1


def parse(x: str) -> list:
    x_ = list(x)
    y = []

    while len(x_) > 0:
        v = x_.pop(0)
        if v in ['n', 's']:
            v += x_.pop(0)
        y.append(v)

    return y


def tadd(t1, t2):
    return tuple(map(lambda x, y: x + y, t1, t2))


def move(ys):
    return reduce(tadd, [POS[y] for y in ys], (0,0,0))


def neighbours(x, y, z) -> set:
    return {(x + dx, y + dy, z + dz) for (dx,dy,dz) in POS.values()}


def initialize_tiling(xs: list) -> dict:
    tiles = defaultdict(lambda: WHITE) # white by default

    for x in xs:
        pos = move(parse(x))
        tiles[pos] = 1 - tiles[pos] # flip

    return dict(tiles)


def stays_black(tile: tuple, black_tiles: set) -> bool:
    black_nbors = len(neighbours(*tile) & black_tiles)

    if tile in black_tiles:
        return not (black_nbors == 0 or black_nbors > 2)
    else:
        return black_nbors == 2


def step(black_tiles: set) -> set:
    expanded = list(chain(*map(lambda t: neighbours(*t), black_tiles)))
    return set(filter(lambda t: stays_black(t, black_tiles) , expanded))


def iterate(xs: list, n: int) -> set:
    tiles = initialize_tiling(xs)
    black_tiles = {t for t, c in tiles.items() if c == BLACK}

    for _ in range(n):
        black_tiles = step(black_tiles)

    return black_tiles


if __name__ == "__main__":
    with open(sys.argv[1], 'rt') as fp:
        xs = fp.read().strip().split('\n')

    print(f'Part 1: {sum(initialize_tiling(xs).values())}')
    print(f'Part 2: {len(iterate(xs, n=100))}')