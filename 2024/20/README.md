---
title: "Day 20: Race Condition"
url: https://adventofcode.com/2024/day/20
tags: grids, pathfinding
---

### Solution
Use a slightly edited version of DFS to get the full path, knowing that it's just a single one, then look for unique pairs `(p1, p2)` of points b/w which a cheat can be inserted and for which the Manhattan distance is `2 <= dist <= 20`. The saved distance is `idx(p2) - idx(p1) - dist(p1, p2)`.

### Usage
```
$ make
```
