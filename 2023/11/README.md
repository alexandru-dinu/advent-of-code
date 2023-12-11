---
title: "Day 11: Cosmic Expansion"
url: https://adventofcode.com/2023/day/11
tags: 2d
---

### Solution
```
Part 1: 9177603
Part 2: 632003913611
```

Representing the grid as a binary numpy array allows for straightforward detection of empty rows and cols.
The shortest distance b/w 2 points is given by Manhattan distance plus checking how many empty rows & cols are b/w the 2 points.

### Usage
```
$ make
```
