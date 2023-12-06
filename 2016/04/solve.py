import re
from collections import Counter
from functools import cmp_to_key
from typing import TextIO


def parse(room: str) -> tuple:
    return re.match(r"([\w-]+)-(\d+)\[(\w+)\]", room).groups()


def check(room: str) -> int:
    enc, sec, chk = parse(room)
    cnt = Counter(enc.replace("-", ""))

    def sorter(a, b):
        sgn = lambda x: x / abs(x)
        if cnt[a] == cnt[b]:
            return sgn(ord(a) - ord(b))
        return sgn(cnt[b] - cnt[a])

    return int(sec) * (chk == "".join(sorted(cnt.keys(), key=cmp_to_key(sorter))[:5]))


def decrypt(enc: str, key: int) -> str:
    def shift(c):
        return " " if c == "-" else chr((ord(c) - ord("a") + key) % 26 + ord("a"))

    return "".join(map(shift, enc))


def solve(fp: TextIO) -> tuple[int, int]:
    rooms = [x.strip() for x in fp]

    p1 = sum(map(check, rooms))

    for enc, sec, _ in map(parse, rooms):
        sec = int(sec)
        if decrypt(enc, sec) == "northpole object storage":
            p2 = sec
            break

    return p1, p2


def test_example() -> None:
    assert 0 < check("aaaaa-bbb-z-y-x-123[abxyz]")
    assert 0 < check("a-b-c-d-e-f-g-h-987[abcde]")
    assert 0 < check("not-a-real-room-404[oarel]")
    assert 0 == check("totally-real-room-200[decoy]")

    assert decrypt("qzmt-zixmtkozy-ivhz", 343) == "very encrypted name"


def main() -> None:
    with open(0) as fp:
        p1, p2 = solve(fp)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
