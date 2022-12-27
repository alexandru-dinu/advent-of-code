import hypothesis.strategies as st
import solve as S
from hypothesis import given, settings


@settings(max_examples=10_000)
@given(st.lists(st.integers(), min_size=1), st.integers(min_value=1))
def test_eq(xs, k):
    assert S.solve1(xs, k) == S.solve2(xs, k) == S.solve3(xs, k)
