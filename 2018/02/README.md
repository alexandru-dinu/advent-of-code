---
title: "Day 2: Inventory Management System"
url: https://adventofcode.com/2018/day/2
tags: strings
---

### Solution
```
Part 1: 7470
Part 2: kqzxdenujwcstybmgvyiofrrd
```

For part 1: use a `Counter` to look for chars appearing twice or thrice.

For part 2: scan all unique pairs of strings and look for the differences, exiting early if more than 1 differing positions are found.

### Usage
```
$ make
```
