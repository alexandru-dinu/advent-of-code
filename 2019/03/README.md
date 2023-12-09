---
title: "Day 3: Crossed Wires"
url: https://adventofcode.com/2019/day/3
tags: 2d
---

### Solution
```
Part 1: 2427
Part 2: 27890
```

Represent segments as:
```
(x, y1) <-> (x, y2): Segment V (y1, y2)
(x1, y) <-> (x2, y): Segment H (x1, x2)
```
Intersections are computed by considering all pairs of segments.
Distance to intersections are computed by traversing the segments and accumulating the lengths until the intersection is reached.

### Usage
```
$ make
```
