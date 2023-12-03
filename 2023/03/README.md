---
title: "Day 3: Gear Ratios"
url: https://adventofcode.com/2023/day/3
tags: 2d
---

### Solution
```
Part 1: 519444
Part 2: 74528807
```

Use regex to find numbers and symbols on each line and populate a grid
with special `Num` and `Sym` values, respectively. The _trick_ is to use the entire number as the member of a cell, to circumvent the need for recomposing the number.

**Part 1:** For each symbol we look at its 8 neighbours and see if we get to a `Num`, in which case we mark it as reachable. Finally, we add all values of the reachable `Num`s.

**Part 2:** For all `*` symbols we look at their 8 neighbours and check if we have exactly 2 `Num`s, in which case we multiply their values and add the result to the total.

### Usage
```
$ make
```
