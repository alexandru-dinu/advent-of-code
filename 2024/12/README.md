---
title: "Day 12: Garden Groups"
url: https://adventofcode.com/2024/day/12
tags: dfs, graphs
---

### Solution
DFS to find continuous regions of plants, followed by 2 different perimeter calculation rules for each of the 2 parts:
1. Add 1 for each side
2. Represent runs of continuous strips (along X or Y) as connected components in a graph; the final perimeter will be the number of connected components.

### Usage
```
$ make
```
