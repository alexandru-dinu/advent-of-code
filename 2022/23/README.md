---
title: "Day 23: Unstable Diffusion"
url: https://adventofcode.com/2022/day/23
tags: cellular-automata
---

### Solution
```
Part 1: 3917
Part 2: 988
```
Cellular automata -> simulate rounds.

The _naive_ implementation is inefficient, as the entire grid in stored in memory (as a boolean mask) with no real purpose. Since the _underlying grid_ is infinite and we only care about occupied positions, we can use a set of complex numbers `x + y * 1j` to denote occupied positions `(y, x)`. This also simplifies working with 2D positions.

### Usage
```
$ make
```
