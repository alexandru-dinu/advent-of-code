#!/usr/bin/env python3

import argparse
import colorsys
import json
import os
import time

import requests


def rgb2hex(r, g, b):
    f = lambda x: max(0, min(255, round(x * 255)))
    return f"{f(r):02x}{f(g):02x}{f(b):02x}"


def hsv_interp(t):
    # 0 - 60 - 120
    assert 0 <= t <= 1
    return rgb2hex(*colorsys.hsv_to_rgb(h=t * 120 / 360, s=1, v=0.6))


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
STAR = "⭐"
NUM_YEARS = 2023 - 2015 + 1


def fmt_year_badge(year: int, stars: int, color: str) -> str:
    return f"https://img.shields.io/badge/{year}-{stars}%20{STAR}-{color}?style=flat-square"


def fmt_total_badge(stars: int, color: str) -> str:
    return f"https://img.shields.io/badge/total-{stars}%20{STAR}-{color}?style=for-the-badge"


def get_year_stars(year: int) -> int:
    res = requests.get(
        AOC_URL.format(year=year, uid=UID),
        headers=HEADERS,
        cookies=COOKIES,
    )
    assert res.status_code == 200
    time.sleep(args.sleep_sec)

    data = json.loads(res.text)

    return data["members"][UID]["stars"]


def get_year_badge_url(year: int, stars: int) -> str:
    color = hsv_interp(stars / 50)

    badge = f'<img src="{fmt_year_badge(year,stars, color)}"></img>'
    if args.link_to_dir:
        badge = f'<a href="./{year}">{badge}</a>'

    return badge


def get_total_badge_url(stars: int) -> str:
    color = hsv_interp(stars / (NUM_YEARS * 50))

    return (
        f'<a href="./README.md"><img src="{fmt_total_badge(stars, color)}"></img></a>'
    )


def main():
    y2s = {y: get_year_stars(y) for y in args.years}

    if args.total_only:
        print(get_total_badge_url(sum(y2s.values())))
    else:
        for y, s in y2s.items():
            print(get_year_badge_url(y, s))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="""
        Generate badge URLs with stars/year.
        The badge color is interpolated with respect to the number of stars: from 0 to 50.
        """.strip(),
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--years",
        nargs="+",
        type=int,
        default=list(range(2023, 2014, -1)),
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
    parser.add_argument(
        "--total-only",
        action="store_true",
        help="Just generate the total number of stars",
    )
    args = parser.parse_args()

    main()
