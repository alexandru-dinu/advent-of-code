"""
2, 1, 0, -, =
- is worth -1
= is worth -2

SNAFU: 1=-0-2
Decimal: 1*5**5 -2*5**4 -1*5**3 + 0*5**2 -1*5**1 +2*5**0 = 1747
"""

from functools import reduce
from itertools import zip_longest
from operator import add


class Snafu:
    S2D = {"2": 2, "1": 1, "0": 0, "-": -1, "=": -2}
    D2S = {2: "2", 1: "1", 0: "0", -1: "-", -2: "="}

    def __init__(self, s):
        self.s = s
        self.digits = [self.S2D[c] for c in s]

    @staticmethod
    def from_digits(digits) -> "Snafu":
        return Snafu(s="".join(Snafu.D2S[d] for d in digits))

    @staticmethod
    def from_decimal(n: int) -> "Snafu":
        raise NotImplementedError

    def to_decimal(self) -> int:
        return sum(d * 5**i for i, d in enumerate(reversed(self.digits)))

    def __add__(self, other) -> "Snafu":
        def step(x) -> tuple[int, int]:
            # return value and carry
            if x >= 3:
                return x - 5, 1
            if x <= -3:
                return x + 5, -1
            assert abs(x) <= 2
            return x, 0

        out = []
        carry = 0
        for a, b in zip_longest(
            reversed(self.digits), reversed(other.digits), fillvalue=0
        ):
            x, carry = step(a + b + carry)
            out.insert(0, x)

        if carry != 0:
            out.insert(0, carry)

        return Snafu.from_digits(out)

    __str__ = __repr__ = lambda self: self.s


def main():
    with open(0) as fp:
        xs = [x.strip() for x in fp]

    print("Result:", reduce(add, map(Snafu, xs)))


if __name__ == "__main__":
    main()
