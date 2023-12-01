---
title: "Day 1: Trebuchet?!"
url: https://adventofcode.com/2023/day/1
tags: search
---

### Solution
```
Part 1: 54601
Part 2: 54078
```
Substring search. Keep a single array of needles: `1-9` & `one-nine` and
perform two searches: leftmost (for first value `a`) and rightmost (for second value `b`).
The result is `10 * a + b`.

For part 1 we look only for `1-9`, whereas for part 2 we also look for `one-nine`.

### Usage
```
$ make
```
