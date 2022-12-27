import sys

from sympy.ntheory.residue_ntheory import discrete_log


def modexp(base, exp, mod=20201227):
    def explicit(base, exp, mod):
        if mod == 1:
            return 0

        res = 1
        base = base % mod

        while exp > 0:
            if exp & 1:
                res = (res * base) % mod
            exp >>= 1
            base = (base * base) % mod

        return res

    # return explicit(base, exp, mod)
    return pow(base, exp, mod)


def find_loop_size(pk, base):
    def bruteforce(pk, base):
        exp = 1
        while True:
            if modexp(base, exp) == pk:
                return exp
            exp += 1

    # return bruteforce(pk, base)
    return discrete_log(20201227, pk, base)


def sample_tests():
    pkc, lsc = 5764801, 8
    pkd, lsd = 17807724, 11

    assert modexp(base=7, exp=lsc) == pkc
    assert modexp(base=7, exp=lsd) == pkd

    assert find_loop_size(pk=pkc, base=7) == lsc
    assert find_loop_size(pk=pkd, base=7) == lsd

    e1 = modexp(base=pkc, exp=lsd)
    e2 = modexp(base=pkd, exp=lsc)
    assert e1 == e2


if __name__ == "__main__":
    with open(sys.argv[1], "rt") as fp:
        pkc, pkd = map(int, fp.read().strip().split("\n"))

    sample_tests()

    lsc = find_loop_size(pkc, base=7)
    lsd = find_loop_size(pkd, base=7)
    enc = modexp(base=pkc, exp=lsd)
    assert 1478097 == enc == modexp(base=pkd, exp=lsc)
    print(f"Part 1: {enc}")
