import numpy as np


def part1(a, b):
    """
    RPS <=> ABC <=> XYZ <=> 012
    win  (a,b) <=> (b-a) % 3 == 1 | score: 6
    lose (a,b) <=> (b-a) % 3 == 2 | score: 0
    draw (a,b) <=> (b-a) % 3 == 0 | score: 3
    """
    # score = shape + outcome
    scores = (b + 1) + ((b - a + 1) % 3 * 3)
    return scores.sum()


def part2(a, b):
    """
    X (0): lose => need (b-a) % 3 == 2
    Y (1): draw => need (b-a) % 3 == 0
    Z (2): win  => need (b-a) % 3 == 1
    => need to find b
    (b-a) % 3 == r
    (b-a) = 3k + r
    b = a + 3k + r
    b = (a + r) % 3
    """
    # score = shape + outcome
    r = (b - 1) % 3  # maps 012 (from XYZ) to 201 (from "need")
    scores = (b * 3) + ((a + r) % 3 + 1)
    return scores.sum()


def main():
    with open(0) as fp:
        moves = [x.split() for x in fp]

    a, b = zip(*moves)
    a = np.array([ord(i) - ord("A") for i in a])
    b = np.array([ord(i) - ord("X") for i in b])

    print("Part 1:", part1(a, b))
    print("Part 2:", part2(a, b))


if __name__ == "__main__":
    main()
