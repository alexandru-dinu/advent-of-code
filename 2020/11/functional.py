from itertools import accumulate, product, repeat


def no_change(prev, curr):
    if prev == curr:
        raise StopIteration
    else:
        return curr


def until(cond, it):
    return accumulate(it, cond)


def iterate(f, x0):
    return accumulate(repeat(x0), lambda lhs, _: f(lhs))


def cartesian(xs):
    return product(xs, repeat=2)
