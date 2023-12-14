---
title: "Day 14: Parabolic Reflector Dish"
url: https://adventofcode.com/2023/day/14
tags: 2d-sim, cycle-detection
---

### Solution
```
Part 1: 110407
Part 2: 87273
```

Similar to [2022/17](https://adventofcode.com/2022/day/17).

Rocks only fall north, so we account for east, west, south orientations by matrix rotations.

Simulation results are cached (by hashing the array) and once a period has been found, we can stop simulating and directly fetch the grid
corresponding to the n-th step (e.g. 1e9).

### Usage
```
$ make
```
