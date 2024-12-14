---
title: "Day 14: Restroom Redoubt"
url: https://adventofcode.com/2024/day/14
tags: sim, stats
---

### Solution
Simulate movements based on position and velocity. To find the Christmas tree for part 2, simulate a large enough number of steps (e.g. 10K) and look for standard deviation of X and Y coordinates, with the assumption that regular patterns will have lower deviations, hence will be more likely to contain the tree. The step where standard deviations of X and Y coordinates are minimized contains the tree.

### Usage
```
$ make
```
