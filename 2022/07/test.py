from bisect import bisect_left, bisect_right

import hypothesis.strategies as st
from hypothesis import given, settings


def my_bisect_left(xs, val):
    if not xs:
        return 0

    lo, hi = 0, len(xs)

    while lo < hi:
        m = lo + (hi - lo) // 2
        if xs[m] < val:
            lo = m + 1
        else:
            hi = m

    return lo


def my_bisect_right(xs, val):
    if not xs:
        return 0

    lo, hi = 0, len(xs)

    while lo < hi:
        m = lo + (hi - lo) // 2
        if xs[m] > val:
            hi = m
        else:
            lo = m + 1

    return lo


@settings(max_examples=100)
@given(st.lists(st.integers(), min_size=0), st.integers())
def test_my_bisect(xs, val):
    assert my_bisect_left(xs, val) == bisect_left(xs, val)
    assert my_bisect_right(xs, val) == bisect_right(xs, val)
