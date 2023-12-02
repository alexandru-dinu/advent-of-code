from bisect import bisect_left
from dataclasses import dataclass, field


@dataclass
class Entry:
    name: str
    parent: str
    is_dir: bool
    size: int = 0
    ls: dict[str, "Entry"] = field(default_factory=dict)

    def to_str(self, indent=0):
        suffix = "/" if self.is_dir and self.name != "/" else ""
        s = " " * indent + f"{self.name}{suffix} ({self.size:,d})" + "\n"
        for x in self.ls:
            s += self.ls[x].to_str(indent=indent + 2)
        return s

    def __str__(self):
        return self.to_str()

    __repr__ = __str__


def parse_tree(cmds: list[str]) -> Entry:
    tree = Entry(name="/", parent=None, is_dir=True)
    tree.parent = tree
    root = tree
    stack: list[Entry] = [tree]

    i = 0
    while i < len(cmds):
        cmd = cmds[i]
        i += 1

        pre, c, *arg = cmd.split()
        assert pre == "$"

        if c == "cd":
            workdir = arg[0]

            if workdir == "..":
                stack.pop()
                workdir = stack[-1]
                tree = tree.parent

            elif workdir != "/":
                stack.append(workdir)
                tree = tree.ls.get(workdir, None)

        elif c == "ls":
            while i < len(cmds) and not cmds[i].startswith("$"):
                info, name = cmds[i].split()
                i += 1

                if info == "dir":
                    tree.ls[name] = Entry(name=name, parent=tree, is_dir=True)
                else:
                    tree.ls[name] = Entry(
                        name=name, parent=tree, is_dir=False, size=int(info)
                    )

        else:
            raise ValueError(f"Unknown command: {cmd}")

    return root


def populate_sizes(tree: Entry):
    if not tree.is_dir:
        return

    for x in tree.ls.values():
        populate_sizes(x)
        tree.size += x.size


# def sum_dir_size(tree):
#     if not tree:
#         return 0

#     if not tree.is_dir:
#         return tree.size

#     total = 0
#     for x in tree.ls.values():
#         total += sum_dir_size(x)

#     return total


def sum_small_dirs(tree: Entry, max_size: int):
    if not tree.is_dir:
        return 0

    total = 0
    for x in tree.ls.values():
        if not x.is_dir:
            continue

        if x.size <= max_size:
            total += x.size

        total += sum_small_dirs(x, max_size)

    return total


def collect_dirs(tree: Entry):
    def inner(tree: Entry, dirs: list[Entry]):
        if not tree.is_dir:
            return

        dirs.append(tree)
        for x in tree.ls.values():
            inner(x, dirs)

    out = []
    inner(tree, out)
    return out


def find_smallest_dir_to_del(tree: Entry, total_size_available: int, need_unused: int):
    unused_size = total_size_available - tree.size

    dirs = sorted(collect_dirs(tree), key=lambda x: x.size)
    sizes = [x.size for x in dirs]

    i = bisect_left([unused_size + s for s in sizes], need_unused)

    return sizes[i]


def main():
    with open(0) as fp:
        cmds = [x.strip() for x in fp]

    tree = parse_tree(cmds)
    populate_sizes(tree)
    # print(tree)

    print("Part 1:", sum_small_dirs(tree, max_size=1e5))
    print(
        "Part 2:",
        find_smallest_dir_to_del(tree, total_size_available=7e7, need_unused=3e7),
    )


if __name__ == "__main__":
    main()
