#!/usr/bin/env python3

import argparse
import json
import os
import time
from math import sqrt

import requests


def rgb2hex(r, g, b):
    f = lambda x: max(0, min(255, round(x * 255)))
    return f"{f(r):02x}{f(g):02x}{f(b):02x}"


def hex2rgb(x):
    return (int(x[:2], 16) / 255, int(x[2:4], 16) / 255, int(x[4:], 16) / 255)


def interp(c0, c1, t):
    x0 = hex2rgb(c0)
    x1 = hex2rgb(c1)
    return rgb2hex(*[(1 - t) * x0[i] + t * x1[i] for i in range(3)])


SID = os.getenv("AOC_SESSION")
assert SID is not None

UID = os.getenv("AOC_UID")
assert UID is not None

DELIM_BEGIN = "<!-- begin-year-badge -->"
DELIM_END = "<!-- end-year-badge -->"

AOC_URL = "https://adventofcode.com/{year}/leaderboard/private/view/{uid}.json"
HEADERS = {
    "User-Agent": "https://github.com/alexandru-dinu/advent-of-code/blob/main/.scripts/gen_badges.py"
}
COOKIES = {"session": SID}
MD_BADGE_URL = "https://img.shields.io/badge/{year}-{stars}%20stars-{color}"


def get_md_urls():
    out = []

    for year in args.years:
        print(f"Fetch data for {year=}")
        res = requests.get(
            AOC_URL.format(year=year, uid=UID),
            headers=HEADERS,
            cookies=COOKIES,
        )
        assert res.status_code == 200
        time.sleep(args.sleep_sec)

        data = json.loads(res.text)

        s = data["members"][UID]["stars"]

        t = sqrt(s / 50)  # sqrt(x) > x, for x in [0, 1], so we get to green faster
        color = interp(args.color0, args.color1, t)

        badge = f"![]({MD_BADGE_URL.format(year=year, stars=s, color=color)})"
        if args.link_to_dir:
            badge = f"[{badge}](./{year})"

        out.append(badge)

    return out


def main():
    with open(args.readme_path, "rt") as fp:
        lines = [x.strip() for x in fp]

    for i, line in enumerate(lines):
        if line == DELIM_BEGIN:
            start = i
        elif line == DELIM_END:
            end = i

    out = lines[: start + 1] + get_md_urls() + lines[end:]

    with open(args.readme_path, "wt") as fp:
        for line in out:
            fp.write(f"{line}\n")

    print(f"{args.readme_path} updated!")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="""
        Generate badges with stars/year.
        The badge color is interpolated with respect to the number of stars: from 0 to 50.
        """.strip(),
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--readme-path",
        type=str,
        default="./README.md",
        help="Path to the README file to edit.",
    )
    parser.add_argument("--color0", type=str, default="ef0f14", help="Start color.")
    parser.add_argument("--color1", type=str, default="239323", help="End color.")
    parser.add_argument(
        "--years",
        nargs="+",
        type=int,
        default=list(range(2022, 2014, -1)),
        help="Years to fetch data from.",
    )
    parser.add_argument(
        "--sleep-sec",
        type=int,
        default=2,
        help="Number of seconds to sleep between requests.",
    )
    parser.add_argument(
        "--link-to-dir",
        action="store_true",
        help="If given, will link the badge to the corresponding `./<year>` directory.",
    )
    args = parser.parse_args()

    main()
