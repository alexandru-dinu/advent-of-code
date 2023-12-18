---
title: "Day 18: Lavaduct Lagoon"
url: https://adventofcode.com/2023/day/18
tags: maths, 2d, geometry
---

### Solution
```
Part 1: 40131
Part 2: 104454050898331
```

For each digging instruction, represent only `start` and `end` points. At the end, the operation is describing a polygon.

Then, the same ideas as in [2023/10](https://github.com/alexandru-dinu/advent-of-code/tree/main/2023/10) can be applied: using [Shoelace formula](https://en.wikipedia.org/wiki/Shoelace_formula) for polygon area and [Pick's theorem](https://en.wikipedia.org/wiki/Pick%27s_theorem) to count the inside points.

Simulation / flood-fill is infeasible for part 2.

### Usage
```
$ make
```
