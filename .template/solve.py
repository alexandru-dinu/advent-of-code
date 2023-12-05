from pathlib import Path
from typing import TextIO


def solve(fp: TextIO) -> tuple[int, int]:
    p1 = ...
    p2 = ...

    return p1, p2


def test_example() -> None:
    with open(Path(__file__).parent / "example") as fp:
        p1, p2 = solve(fp)

    assert p1 == ...
    assert p2 == ...


def main() -> None:
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
