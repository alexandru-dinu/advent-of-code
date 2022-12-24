---
title: "Day 8: Treetop Tree House"
url: https://adventofcode.com/2022/day/8
---

### Solution
```
Part 1: 1870
Part 2: 517440
```
Yield the up / down / left / right views and solve solve both parts in one traversal,
i.e. for each inner tree:
- part 1: count cases s.t. there's `any` view where `all` other trees are shorter than it
- part 2: multiply the number of visible trees from `all` views, then find the max

### Usage
```
$ make
```
