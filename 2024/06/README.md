---
title: "Day 6: Guard Gallivant"
url: https://adventofcode.com/2024/day/6
tags: grids
---

### Solution
Store the grid coords as complex numbers for easy walking. For part 1, simply walk until falling out of the grid.
For part 2, place a new obstacle on the path from part 1 and check if we're ending up in a loop.
Loop checking is done by testing whether a position and orientation tuple has already been seen.

Improvements:
- store the entire grid (not just the obstacles) as complex positions to not require bound checks
- have a single `walk` function that generates the path and tells if it contains a cycle

### Usage
```
$ make
```
