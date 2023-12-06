DIRS = {">": +1, "<": -1, "^": +1j, "v": -1j}


def solve(dirs: list[str], num: int) -> int:
    """At least 1 visit."""
    zs = [0] * num
    vis = {0}

    for i, d in enumerate(dirs):
        k = i % num
        zs[k] += DIRS[d]
        vis.add(zs[k])

    return len(vis)


def test_example() -> None:
    assert solve(">", num=1) == 2
    assert solve("^>v<", num=1) == 4
    assert solve("^v^v^v^v^v", num=1) == 2

    assert solve("^v", num=2) == 3
    assert solve("^>v<", num=2) == 3
    assert solve("^v^v^v^v^v", num=2) == 11


def main() -> None:
    with open(0) as fp:
        dirs = fp.read().strip()

    for i in range(1, 3):
        print(f"Part {i}: {solve(dirs, num=i)}")


if __name__ == "__main__":
    main()
