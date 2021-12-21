import sys
from dataclasses import dataclass
from itertools import product


def mod(x):
    return (x - 1) % 10 + 1


def move(p):
    s, t = 0, 1
    while True:
        s += mod(p(t))
        t += 1
        yield s


def part1(start1: int, start2: int) -> int:
    score1 = move(p=lambda t: start1 + 9 * t ** 2 - 3 * t)
    score2 = move(p=lambda t: start2 + 9 * t ** 2 + 6 * t)
    rolls = 0

    while True:
        s1 = next(score1)
        rolls += 3
        if s1 >= 1000:
            break

        s2 = next(score2)
        rolls += 3
        if s2 >= 1000:
            break

    return min(s1, s2) * rolls


@dataclass(frozen=True)
class State:
    p_cur: int
    s_cur: int
    p_oth: int
    s_oth: int


# ... or cache from functools
def part2(start1: int, start2: int) -> int:
    mem = {}

    def inner(state: State) -> tuple[int, int]:
        if state.s_cur >= 21:
            return (1, 0)
        if state.s_oth >= 21:
            return (0, 1)
        if (m := mem.get(state)) is not None:
            return m

        cnt = [0, 0]

        for dice in product([1, 2, 3], repeat=3):
            p_new = mod(state.p_cur + sum(dice))
            s_new = state.s_cur + p_new

            # switch players and go recursively
            state_new = State(state.p_oth, state.s_oth, p_new, s_new)

            o, c = inner(state_new)
            cnt[0] += c
            cnt[1] += o

        mem[state] = cnt
        return cnt

    wins1, wins2 = inner(State(p_cur=start1, s_cur=0, p_oth=start2, s_oth=0))
    return max(wins1, wins2)


def main():
    with open(sys.argv[1]) as fp:
        s1, s2 = map(lambda x: int(x.split(": ")[1]), fp.read().strip().split("\n"))

    print("Part 1:", part1(s1, s2))
    print("Part 2:", part2(s1, s2))


if __name__ == "__main__":
    main()
