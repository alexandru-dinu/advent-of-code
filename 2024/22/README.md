---
title: "Day 22: Monkey Market"
url: https://adventofcode.com/2024/day/22
tags: arrays
---

### Solution
Part 1 is just implementing the transformation rules and iterating them.

For part 2, keep a dict `changes -> {buyer -> value}`, then look for the `changes` with the max `sum over buyer's values`.

I initially overlooked the statement and considered _all_ `changes` occurrences for each buyer, but should stop at the first one, e.g. in `..., a,b,c,d, ..., a,b,c,d, ...` only the first `a,b,c,d` `changes` should be considered.

### Usage
```
$ make
```
