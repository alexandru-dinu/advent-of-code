#!/usr/bin/env python3

import json
import os
import time

import matplotlib as mpl
import numpy as np
import requests
from tqdm import trange

SID = os.getenv("AOC_SESSION")
assert SID is not None

UID = os.getenv("AOC_UID")
assert UID is not None

C_RED = np.array(mpl.colors.to_rgb("#ef0f14"))
C_GRN = np.array(mpl.colors.to_rgb("#239323"))

AOC_URL = "https://adventofcode.com/{year}/leaderboard/private/view/{uid}.json"
MD_BADGE_URL = "https://img.shields.io/badge/{year}-{stars}%20stars-{color}"


def get_md_urls():
    out = []

    for year in trange(2022, 2014, -1):
        res = requests.get(AOC_URL.format(year=year, uid=UID), cookies={"session": SID})
        assert res.status_code == 200
        time.sleep(2)

        data = json.loads(res.text)

        s = data["members"][UID]["stars"]
        t = np.sqrt(s / 50)
        c = mpl.colors.to_hex((1 - t) * C_RED + t * C_GRN).strip("#")

        md_str = f"[![]({MD_BADGE_URL.format(year=year, stars=s, color=c)})](./{year})"
        out.append(md_str)

    return out


with open("README.md", "rt") as fp:
    lines = [x.strip() for x in fp]

for i, line in enumerate(lines):
    if line == "<!-- begin-year-badge -->":
        start = i
    elif line == "<!-- end-year-badge -->":
        end = i

out = lines[: start + 1] + get_md_urls() + lines[end:]

with open("README.md", "wt") as fp:
    for line in out:
        fp.write(f"{line}\n")
