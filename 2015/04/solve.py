from hashlib import md5
from itertools import count


def find_prefix(key: str, prefix: str) -> int:
    for x in count(1):
        d = md5(f"{key}{x}".encode()).hexdigest()
        if d.startswith(prefix):
            return x


def solve(key: str) -> tuple[int, int]:
    p1 = find_prefix(key, prefix="0" * 5)
    p2 = find_prefix(key, prefix="0" * 6)
    return p1, p2


def test_example() -> None:
    assert solve("abcdef") == (609043, 6742839)
    assert solve("pqrstuv") == (1048970, 5714438)


def main() -> None:
    with open(0) as fp:
        p1, p2 = solve(fp.read().strip())

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
