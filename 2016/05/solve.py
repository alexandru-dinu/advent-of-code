from hashlib import md5
from itertools import count
from typing import TextIO


def gen_valid(key):
    for x in count(0):
        if (h := md5(f"{key}{x}".encode()).hexdigest()).startswith("0" * 5):
            yield h


def find1(key: str):
    g = gen_valid(key)
    return "".join(next(g)[5] for _ in range(8))


def find2(key: str):
    s = [None] * 8
    k = 0

    for h in gen_valid(key):
        i, c = h[5:7]
        if i.isdigit() and 0 <= (j := int(i)) < 8 and s[j] is None:
            s[j] = c
            if (k := k + 1) == 8:
                break

    return "".join(s)


def solve(fp: TextIO) -> tuple[str, str]:
    key = fp.read().strip()

    return find1(key), find2(key)


def test_example() -> None:
    assert find1("abc") == "18f47a30"
    assert find2("abc") == "05ace8e3"


def main() -> None:
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
