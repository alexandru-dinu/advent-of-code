#!/usr/bin/env python3

import json
import os
import time
from math import sqrt

import requests


def rgb2hex(r, g, b):
    f = lambda x: max(0, min(255, round(x * 255)))
    return f"{f(r):2x}{f(g):2x}{f(b):2x}"


def hex2rgb(x):
    return (int(x[:2], 16) / 255, int(x[2:4], 16) / 255, int(x[4:], 16) / 255)


SID = os.getenv("AOC_SESSION")
assert SID is not None

UID = os.getenv("AOC_UID")
assert UID is not None

AOC_URL = "https://adventofcode.com/{year}/leaderboard/private/view/{uid}.json"
MD_BADGE_URL = "https://img.shields.io/badge/{year}-{stars}%20stars-{color}"

README_PATH = "./README.md"

C_RED = hex2rgb("ef0f14")
C_GRN = hex2rgb("239323")


def get_md_urls():
    out = []

    for year in range(2022, 2014, -1):
        print(f"Fetch data for {year=}")
        res = requests.get(AOC_URL.format(year=year, uid=UID), cookies={"session": SID})
        assert res.status_code == 200
        time.sleep(2)

        data = json.loads(res.text)

        s = data["members"][UID]["stars"]
        t = sqrt(s / 50)  # sqrt(x) > x, for x in [0, 1], so we get to green faster
        color = rgb2hex(*[(1 - t) * C_RED[c] + t * C_GRN[c] for c in range(3)])

        out.append(
            f"[![]({MD_BADGE_URL.format(year=year, stars=s, color=color)})](./{year})"
        )

    return out


with open(README_PATH, "rt") as fp:
    lines = [x.strip() for x in fp]

for i, line in enumerate(lines):
    if line == "<!-- begin-year-badge -->":
        start = i
    elif line == "<!-- end-year-badge -->":
        end = i

out = lines[: start + 1] + get_md_urls() + lines[end:]

with open(README_PATH, "wt") as fp:
    for line in out:
        fp.write(f"{line}\n")

print(f"{README_PATH} updated!")
