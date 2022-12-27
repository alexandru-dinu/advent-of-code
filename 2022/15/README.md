---
title: "Day 15: Beacon Exclusion Zone"
url: https://adventofcode.com/2022/day/15
tags: searching, constraint-satisfaction
---

### Solution
```
Part 1: 4883971
Part 2: 12691026767556
```
For part 1, find the `lo` and `hi` bounds given the radii of the leftmost and rightmost scanners,
then iterate through all `x` points at the given `y_offset` and check whether the candidate point is
in the range of any sensor, in which case we increment the counter. This search is done in parallel by splitting
the search space in batches.

For part 2, we follow a very neat [idea][1] of phrasing the problem as
a constraint satisfaction problem and using Z3 to find the only solution.
The constraints are the bounds and the fact that there is a single beacon not found by any sensor.

Other idea: using [rot45][2] (easy with complex numbers) to get a square radius instead
of the diamond.

[1]: https://www.reddit.com/r/adventofcode/comments/zmcn64/comment/j0af5cy
[2]: https://www.reddit.com/r/adventofcode/comments/zmcn64/comment/j0ayws9

### Usage
```
$ make
```
