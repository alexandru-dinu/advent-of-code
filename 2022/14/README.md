---
title: "Day 14: Regolith Reservoir"
url: https://adventofcode.com/2022/day/14
tags: 2d-sim, cellular-automata
---

### Solution
```
Part 1: 696
Part 2: 23610
```
Simulate the sand particles, similar to a cellular automaton.

Part 2 is pretty inefficient, since the grid is being grown as the particles fall out of sight.

I think a more efficient solution for part 2 would be to _cast a ray of light_ from the source
to the floor (which will form a triangle), compute its area, then subtract the _shadowed_ regions.

### Usage
```
$ make
```
