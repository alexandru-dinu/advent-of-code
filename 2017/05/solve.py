from pathlib import Path
from typing import TextIO


class State(int):
    """
    Fun idea: custom int wrapper keeping track of `__iadd__` calls.
    """

    cnt = 0

    def __iadd__(self, other):
        State.cnt += 1
        return State(self + other)

    def __enter__(self):
        State.cnt = 0
        return self

    __exit__ = lambda self, *_, **__: _


class Temp:
    def __init__(self, obj):
        self.obj = obj
        assert hasattr(obj, "copy")

    def __enter__(self):
        return self.obj.copy()

    __exit__ = lambda self, *_, **__: _


def solve(fp: TextIO):
    xs = [int(x.strip()) for x in fp]

    with Temp(xs) as _xs, State(0) as z:
        while 0 <= z < len(_xs):
            old = z
            z += _xs[z]
            _xs[old] += 1
    p1 = State.cnt

    with Temp(xs) as _xs, State(0) as z:
        while 0 <= z < len(_xs):
            old = z
            z += _xs[z]
            _xs[old] = _xs[old] - 1 if _xs[old] >= 3 else _xs[old] + 1
    p2 = State.cnt

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == 5
    assert p2 == 10


def main():
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
