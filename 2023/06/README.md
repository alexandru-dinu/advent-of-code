---
title: "Day 6: Wait For It"
url: https://adventofcode.com/2023/day/6
tags: maths
---

### Solution
```
Part 1: 281600
Part 2: 33875953
```

Assume `x` is the time spent charging the boat (to speed `x`), then we have `(t - x)` time left for the race, to travel more than `d` distance. We need to solve `x * (t - x) > d` on the integers.

### Usage
```
$ make
```
