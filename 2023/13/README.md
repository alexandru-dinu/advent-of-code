---
title: "Day 13: Point of Incidence"
url: https://adventofcode.com/2023/day/13
tags: 2d, arrays
---

### Solution
```
Part 1: 29213
Part 2: 37453
```

Very similar to [2021/13](https://adventofcode.com/2021/day/13).

Represent the grid as a binary numpy array: `# == 1`, `. == 0`, then scan for vertical and horizontal symmetries (i.e. where the sub-matrices overlap) looking for exact `diff` mismatches (0 for part 1 and 1 for part 2).

### Usage
```
$ make
```
