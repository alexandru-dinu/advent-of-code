---
title: "Day 18: Boiling Boulders"
url: https://adventofcode.com/2022/day/18
tags: 3d, bfs
---

### Solution
```
Part 1: 4482
Part 2: 2576
```
- Part 1
    - 1/ compute a pairwise manhattan distance matrix `d[i,j]` and see how many pairs
    have `dist == 1` (i.e. they share a face); then, from the max number of faces `6*N`
    we can subtract `2*(dist==1)`.
    - 2/ alternatively, for each cube of the droplet we can count how many of its faces are exposed
- Part 2:
We have that:
```
(bounding box) - (reachable by water) = (droplet cubes) + (trapped air)
```
So we can _fill_ the droplet's trapped air cubes and reuse the surface area computation for part 1.
That is, we now compute the surface area for `(droplet cubes) + (trapped air)`.

### Usage
```
$ make
```
