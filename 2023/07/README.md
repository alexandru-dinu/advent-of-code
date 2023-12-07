---
title: "Day 7: Camel Cards"
url: https://adventofcode.com/2023/day/7
tags: strings
---

### Solution
```
Part 1: 253954294
Part 2: 254837398
```

Represent hand-checking using a `Counter`.
For part 2, the `J` is iteratively replaced with cards in decreasing order of their value, so the highest possible hand is matched.
The list of hands is then sorted using a custom comparator.

### Usage
```
$ make
```
