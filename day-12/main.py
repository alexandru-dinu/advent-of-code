import re
import sys


class Ship:
    __turns = [(1, 0), (0, 1), (-1, 0), (0, -1)]

    def __init__(self, pos, ori):
        self.posX, self.posY = pos
        self.oriX, self.oriY = ori

    def move(self, cmd, num):
        if cmd == 'F':
            self.posX += self.oriX * num
            self.posY += self.oriY * num

        if cmd == 'N':
            self.posY += num
        if cmd == 'S':
            self.posY -= num
        if cmd == 'E':
            self.posX += num
        if cmd == 'W':
            self.posX -= num

        # can only have {90, 180, 270}
        num //= 90
        i = self.__turns.index((self.oriX, self.oriY))
        if cmd == 'L':
            self.oriX, self.oriY = self.__turns[(i + num) % 4]
        if cmd == 'R':
            self.oriX, self.oriY = self.__turns[(i + 4-num) % 4]


if __name__ == "__main__":
    with open(sys.argv[1], 'rt') as fp:
        cmds = [l.strip() for l in fp.readlines()]
        cmds = [re.match(r'([NSEWLRF])(\d+)', c).groups() for c in cmds]
        cmds = [(d, int(n)) for (d, n) in cmds]

    ship = Ship(pos=(0, 0), ori=(1, 0))

    for cmd, num in cmds:
        ship.move(cmd, num)

    print(f'Part 1: {abs(ship.posX) + abs(ship.posY)}')