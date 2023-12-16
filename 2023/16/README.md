---
title: "Day 16: The Floor Will Be Lava"
url: https://adventofcode.com/2023/day/16
tags: 2d-sim
---

### Solution
```
Part 1: 7543
Part 2: 8231
```

Each beam has a position and an orientation, both represented as complex numbers. Depending on the type of the current cell, we decide
what the next set of beams should be (e.g. either simply move the beam according to its orientation, or split it in two beams).
The beams are added to a `visited` set (so if the same position and orientation  is encountered again, we skip this beam) which allows for termination.
For part 2, the search starts from all edge points.

### Usage
```
$ make
```
