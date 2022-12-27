---
title: "Day 24: Blizzard Basin"
url: https://adventofcode.com/2022/day/24
tags: path-finding, A*, dynamic
---

### Solution
```
Part 1: 230
Part 2: 713
```
Dynamic A\*. States are `(time, position)`. Priority for next states is defined as: `time + manh(s, end)`.

To deal with the dynamic nature of the problem,
I am maintaining a cache of grids indexed by the time step (length is upper bounded by the max time step).
That is, when exploring states at time step `t` use `grids[t]`. The next grids are yielded as needed from a generator.

The grid is stored as `pos: [occupied]`, where `occupied` can be either blizzard (`^v<>`) or wall (`#`).

### Usage
```
$ make
```
