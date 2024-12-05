---
title: "Day 5: Print Queue"
url: https://adventofcode.com/2024/day/5
tags: arrays
---

### Solution
For part 1, iterate through all the `X|Y` rules present in the array under test and check that their order in the array is correct.
For part 2, repeatedly swap misplaced rules in the array until the array is "sorted".

Initially tried to represent the rules `X|Y` (`X` comes before `Y`) as edges in a directed graph `X -> Y`.
Essentially asking whether `X < Y` by `has_path(X, Y)`.
However, this approach yielded a large number of false positives, incorrectly marking an array as "sorted", not sure why..

### Usage
```
$ make
```
