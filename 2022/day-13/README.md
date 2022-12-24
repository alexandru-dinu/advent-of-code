---
title: "Day 13: Distress Signal"
url: https://adventofcode.com/2022/day/13
---

### Solution
```
Part 1: 6415
Part 2: 20056
```
Implement a custom packet comparator, returning `LT (-1), EQ (0), GT (1)`.

For part 2, we can sort the entire list leveraging [`cmp_to_key`][1], then multiply the indices
of the divider packets.

However, the alternative implemented here is to partition the list w.r.t.
the divider packets (as done for quicksort) and multiply the indices (which will correspond to the final indices in a sorted list).

[1]: https://docs.python.org/3/library/functools.html#functools.cmp_to_key

### Usage
```
$ make
```
