from itertools import starmap


def paper_area(l, w, h):
    s1, s2, _ = sorted([l, w, h])
    return 2 * (l * w + w * h + l * h) + (s1 * s2)


def ribbon_length(l, w, h):
    s1, s2, _ = sorted([l, w, h])
    return 2 * (s1 + s2) + l * w * h


def main():
    with open(0) as fp:
        dims = [tuple(map(int, line.split("x"))) for line in fp]

    print("Part 1:", sum(starmap(paper_area, dims)))
    print("Part 2:", sum(starmap(ribbon_length, dims)))


if __name__ == "__main__":
    main()
