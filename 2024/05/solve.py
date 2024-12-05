from pathlib import Path
from typing import TextIO


def is_ok(xs, rules):
    for a, b in rules:
        if a in xs and b in xs:
            if xs.index(a) > xs.index(b):
                return False

    return True


def make_ok(xs, rules):
    while not is_ok(xs, rules):
        for a, b in rules:
            if a in xs and b in xs:
                if (i := xs.index(a)) > (j := xs.index(b)):
                    xs[i], xs[j] = xs[j], xs[i]

    return xs


def solve(fp: TextIO):
    rules, updates = fp.read().strip().split("\n\n")
    rules = [(int(x[0]), int(x[1])) for line in rules.split("\n") if (x := line.strip().split("|"))]
    updates = [list(map(int, line.split(","))) for line in updates.split("\n")]

    assert set(sum(updates, [])) <= set(sum(rules, ()))

    ok, nok = [], []
    for xs in updates:
        [nok.append, ok.append][is_ok(xs, rules)](xs)

    p1 = sum(xs[len(xs) // 2] for xs in ok)
    p2 = sum(xs[len(xs) // 2] for xs in map(lambda xs: make_ok(xs, rules), nok))

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 143
    assert p2 == 123


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
