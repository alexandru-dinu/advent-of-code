from hypothesis import given, settings
from hypothesis import strategies as st

from solve import Snafu

DIGITS = list(Snafu.S2D.keys())


@settings(max_examples=10_000)
@given(
    st.text(st.sampled_from(DIGITS), min_size=1),
    st.text(st.sampled_from(DIGITS), min_size=1),
)
def test_add(x: str, y: str):
    sx, sy = Snafu(x), Snafu(y)
    assert sx.to_decimal() + sy.to_decimal() == (sx + sy).to_decimal()
