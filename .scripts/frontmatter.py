import argparse
from collections import Counter
from itertools import takewhile
from pathlib import Path

import yaml


def get_frontmatter(f: str) -> dict:
    with open(f, "rt") as fp:
        assert next(fp).startswith("---")
        fm = "".join(takewhile(lambda x: not x.startswith("---"), fp))

    return yaml.safe_load(fm)


def get_tags(fm: dict) -> list[str]:
    return [x.strip() for x in fm["tags"].split(",")]


def main():
    fm = Counter()
    for f in args.files:
        tags = get_tags(get_frontmatter(f))
        fm.update(tags)

    print(fm.most_common())


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-f", "--files", type=Path, nargs="+", help="Paths to md files."
    )
    args = parser.parse_args()

    main()
