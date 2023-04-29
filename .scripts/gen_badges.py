#!/usr/bin/env python3

import argparse
import json
import os
import time

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


# cookie session (see browser tools)
SID = os.getenv("AOC_SESSION")
assert SID is not None

# personal ID (see in AOC Settings)
UID = os.getenv("AOC_UID")
assert UID is not None

AOC_URL = "https://adventofcode.com/{year}/leaderboard/private/view/{uid}.json"
HEADERS = {
    "User-Agent": "https://github.com/alexandru-dinu/advent-of-code/blob/main/.scripts/gen_badges.py"
}
COOKIES = {"session": SID}
MD_BADGE_URL = "https://img.shields.io/badge/{year}-{stars}%20stars-{color}"


def get_badge_urls():
    out = []

    for year in args.years:
        # print(f"Fetch data for {year=}")
        res = requests.get(
            AOC_URL.format(year=year, uid=UID),
            headers=HEADERS,
            cookies=COOKIES,
        )
        assert res.status_code == 200
        time.sleep(args.sleep_sec)

        data = json.loads(res.text)

        s = data["members"][UID]["stars"]

        # t = sqrt(s / 50)  # sqrt(x) > x, for x in [0, 1], so we get to green faster
        t = s / 50
        color = interp(args.color0, args.color1, t)

        badge = (
            f'<img src="{MD_BADGE_URL.format(year=year, stars=s, color=color)}"></img>'
        )
        if args.link_to_dir:
            badge = f'<a href="./{year}">{badge}</a>'

        out.append(badge)

    return out


def main():
    for url in get_badge_urls():
        print(url)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="""
        Generate badge URLs with stars/year.
        The badge color is interpolated with respect to the number of stars: from 0 to 50.
        """.strip(),
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
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
