import sys
import re
from typing import List


mask_re = re.compile(r'mask = ([01X]+)')
mem_re  = re.compile(r'mem\[(\d+)\] = (\d+)')


def execute(prog: List[str], writer: callable) -> dict:
    mem  = {}
    mask = None

    for line in prog:
        if m := mask_re.match(line):
            mask = m.group(1)

        elif m := mem_re.match(line):
            addr, val = map(int, m.groups())
            writer(mem, addr, val, mask)

        else:
            raise ValueError(f'Invalid instruction: {line}')

    return mem


def part1(mem: dict, addr: int, val: int, mask: str) -> None:
    """
    0/1 overwrites the bit in `val`, X leaves the bit unchanged.
    1) & (X->1) to copy input; ensures (mask=0)->(out=0)
    2) | (X->0) to ensure (mask=1)->(out=1); rest is unchanged
    """
    m0 = int(mask.replace('X', '0'), 2)
    m1 = int(mask.replace('X', '1'), 2)
    mem[addr] = (val & m1) | m0


def part2(mem: dict, addr: int, val: int, mask: str) -> None:
    """
    0/1 -> unchanged,
    X   -> overwrite with 2**count(X) combs.
    """
    addr |= int(mask.replace('X', '0'), 2)
    idx = [i for i,m in enumerate(mask) if m == 'X']
    nX = mask.count('X')

    for i in range(2 ** nX):
        repl  = f'{i:b}'.zfill(nX)
        addr_ = list(f'{addr:036b}')
        for j, r in zip(idx, repl):
            addr_[j] = r
        addr_ = int(''.join(addr_), 2)

        mem[addr_] = val


if __name__ == "__main__":
    with open(sys.argv[1], 'rt') as fp:
        prog = fp.read().strip().split('\n')

    mem = execute(prog, writer=part1)
    print(f'Part 1: {sum(mem.values())}')

    mem = execute(prog, writer=part2)
    print(f'Part 2: {sum(mem.values())}')