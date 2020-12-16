import re
import sys


def move_ship(from_pos: tuple, from_ori: tuple, cmds: list) -> tuple:
    turns = [(1, 0), (0, 1), (-1, 0), (0, -1)]

    posX, posY = from_pos
    oriX, oriY = from_ori

    for cmd, n in cmds:
        if cmd == 'F':
            posX += oriX * n
            posY += oriY * n

        if cmd == 'N':
            posY += n
        if cmd == 'S':
            posY -= n
        if cmd == 'E':
            posX += n
        if cmd == 'W':
            posX -= n

        # can only have {90, 180, 270}
        n //= 90
        i = turns.index((oriX, oriY))
        if cmd == 'L':
            oriX, oriY = turns[(i + n) % 4]
        if cmd == 'R':
            oriX, oriY = turns[(i + 4-n) % 4]

    return posX, posY


def move_ship_complex(p: complex, o: complex, cmds: list) -> tuple:
    dirs = {'N': +1j, 'S': -1j, 'E': +1, 'W': -1}

    for cmd, n in cmds:
        if cmd == 'F':
            p += o * n
        elif cmd == 'L':
            o *= 1j ** (n//90)
        elif cmd == 'R':
            o *= 1j ** (-n//90)
        else:
            p += n * dirs[cmd]

    return p.real, p.imag


def mh(p: tuple):
    return abs(p[0]) + abs(p[1])


if __name__ == "__main__":
    with open(sys.argv[1], 'rt') as fp:
        cmds = [l.strip() for l in fp.readlines()]
        cmds = [re.match(r'([NSEWLRF])(\d+)', c).groups() for c in cmds]
        cmds = [(d, int(n)) for (d, n) in cmds]

    pos = move_ship(from_pos=(0,0), from_ori=(1, 0), cmds=cmds)
    print(f'Part 1: {mh(pos)}')

    pos = move_ship_complex(p=0+0j, o=1+0j, cmds=cmds)
    print(f'Part 1 (using complex numbers): {mh(pos)}')
