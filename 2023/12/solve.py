from itertools import starmap
from pathlib import Path
from typing import TextIO

PatternT = str
TargetT = tuple[int, ...]
InputT = tuple[PatternT, TargetT]


def get_inp(fp: TextIO) -> list[InputT]:
    xss = []

    for pat, tgt in map(lambda line: line.split(), fp):
        xss.append((pat, tuple(int(x) for x in tgt.split(","))))

    return xss


def mk_part2_inp(pat: PatternT, tgt: TargetT) -> InputT:
    return "?".join([pat] * 5), tgt * 5


def solve_naive(pat: PatternT, tgt: TargetT) -> int:
    def _counts(pat) -> tuple[int, ...]:
        return tuple(k.count("#") for k in "".join(pat).split(".") if k != "")

    def _inner(pat: list[str], i: int) -> int:
        if "?" not in pat:
            assert i <= len(pat)
            return tgt == _counts(pat)

        # first `?` in the pattern
        i = pat.index("?")

        if i >= len(pat):
            return 0

        cnt = 0

        a = pat.copy()
        a[i] = "#"
        cnt += _inner(a, i + 1)

        a = pat.copy()
        a[i] = "."
        cnt += _inner(a, i + 1)

        return cnt

    return _inner(list(pat), i=0)


def memoize(func):
    cache = {}

    def inner(*args):
        if args not in cache:
            cache[args] = func(*args)

        return cache[args]

    return inner


@memoize
def solve_smart(pat: PatternT, tgt: TargetT) -> int:
    if not tgt:
        # no more targets, so we check if all that is left is either `?` or `.`
        # all potential `?` are implicitly converted to `.`
        return int(pat.count("#") == 0)

    if not pat:
        return int(sum(tgt) == 0)

    x, xs = pat[0], pat[1:]

    if x == ".":
        # we are currently on a `.`, can`t do anything but advance the pattern and keep the same target
        return solve_smart(xs, tgt)

    if x == "#":
        n, ns = tgt[0], tgt[1:]

        # we are currently on a `#` and we must have a continuous run of `#`
        # to satisfy the current group n
        if n != pat[:n].replace("?", "#").count("#"):
            return 0

        # if this is the final run, we get a new valid combination only if there are no other groups
        if n == len(pat):
            return int(len(ns) == 0)

        # if the first char outside this current group is `?` or `.`,
        # then we can safely "ignore" (treat `?` as `.`) and
        # continue with the rest of the pattern and the rest of the groups
        if pat[n] in "?.":
            return solve_smart(pat[n + 1 :], ns)

        return 0

    assert x == "?"
    # treat `?` as `.` (i.e. skip) & `#`
    return solve_smart(xs, tgt) + solve_smart(f"#{xs}", tgt)


def solve(fp: TextIO):
    xss = get_inp(fp)

    # p1 = sum(starmap(solve_naive, xss))
    p1 = sum(starmap(solve_smart, xss))

    xss = list(starmap(mk_part2_inp, xss))
    p2 = sum(starmap(solve_smart, xss))

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 21
    assert p2 == 525152


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
