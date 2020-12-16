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


def move_ship_complex(cmds, pos, ori, use_waypoint: bool) -> complex:
    dirs = {'N': +1j, 'S': -1j, 'E': +1, 'W': -1}

    for cmd, n in cmds:
        if cmd == 'F':
            pos += n * ori
        elif cmd == 'L':
            ori *= 1j ** (n//90)
        elif cmd == 'R':
            ori *= 1j ** (-n//90) # or 4 - n//90 to make use of the L-rot
        else:
            if use_waypoint:
                ori += n * dirs[cmd] # move waypoint
            else:
                pos += n * dirs[cmd] # move self

    return pos


def mh(x, y):
    return abs(x) + abs(y)


if __name__ == "__main__":
    with open(sys.argv[1], 'rt') as fp:
        cmds = [l.strip() for l in fp.readlines()]
        cmds = [re.match(r'([NSEWLRF])(\d+)', c).groups() for c in cmds]
        cmds = [(d, int(n)) for (d, n) in cmds]

    px, py = move_ship(from_pos=(0,0), from_ori=(1, 0), cmds=cmds)
    print(f'Part 1: {mh(px, py)}')

    p = move_ship_complex(cmds, pos=0+0j, ori=1+0j, use_waypoint=False)
    print(f'Part 1 (using complex numbers): {mh(p.real, p.imag)}')

    p = move_ship_complex(cmds, pos=0+0j, ori=10+1j, use_waypoint=True)
    print(f'Part 2 (using complex numbers): {mh(p.real, p.imag)}')