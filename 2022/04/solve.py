import re
from itertools import starmap


def complete_overlap(a, b, c, d):
    return (a <= c <= d <= b) or (c <= a <= b <= d)


def partial_overlap(a, b, c, d):
    return (a <= c <= b) or (c <= a <= d)


def main():
    with open(0) as fp:
        # format is "a-b,c-d"
        xs = [tuple(map(int, re.split(r"[,-]", x))) for x in fp]

    print("Part 1:", sum(starmap(complete_overlap, xs)))
    print("Part 2:", sum(starmap(partial_overlap, xs)))


if __name__ == "__main__":
    main()
