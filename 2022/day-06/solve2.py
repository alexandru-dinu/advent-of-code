import sys
from collections import deque


def search(fp, target):
    window = deque(fp.read(target))

    while len(set(window)) != target:
        window.popleft()
        window.append(fp.read(1))

    return fp.tell()


def main():
    with open(sys.argv[1]) as fp:
        print("Part 1:", search(fp, target=4))
        fp.seek(0)
        print("Part 2:", search(fp, target=14))


if __name__ == "__main__":
    main()
