---
title: "Day 17: Chronospatial Computer"
url: https://adventofcode.com/2024/day/17
tags: interpreter, reverse-engineering, csp
---

### Solution
Part 1 is a simple interpreter.

Part 2 observations:
- order of magnitude of register `A` controls length of the output (due to repeated idiv by 8)
- tried to [look for patterns](https://docs.google.com/spreadsheets/d/1ypgLxsjrR8vDnOzb9cPjTsYPe29sBd-v8BKn1EQIb4g/edit?usp=sharing), but didn't find anything obvious

So I figured the search can be expressed as a constraint satisfaction problem and use z3 to solve it.
I first converted the program to actual python code manipulating A, B, C registers, then I defined constraints based on the program output and expected output.
To minimize the value of A, a repeated model is searched while adding "less than" constraints (i.e. the new A should be less than the prev A).

### Usage
```
$ make
```
