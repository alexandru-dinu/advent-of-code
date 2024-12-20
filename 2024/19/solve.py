from bisect import bisect
from pathlib import Path

from tqdm import tqdm


def mycache(func):
    mem = {}

    def _inner(*args):
        if args not in mem:
            mem[args] = func(*args)
            # print(f'+++ caching {args=}; {mem[args]=}')
        # else:
            # print(f'>>> reusing {args=}; {mem[args]=}')
        return mem[args]

    return _inner


def search(test, instr):
    ins = sorted([(len(i), i) for i in instr])

    @mycache
    def _inner(cur):
        res = 0
        if not cur:
            return 1

        k = bisect(ins, (len(cur), chr(ord("z") + 1)))
        for n, new_prefix in ins[:k]:
            if cur.startswith(new_prefix):
                rem = cur[n:]
                res += _inner(rem)

        return res

    return _inner(test)


def solve(fp):
    instr, tests = fp.read().strip().split("\n\n")
    instr = instr.split(", ")
    tests = tests.split("\n")

    res = [search(t, instr) for t in tqdm(tests)]
    p1 = sum(x > 0 for x in res)
    p2 = sum(res)
    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 6
    assert p2 == 16


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
