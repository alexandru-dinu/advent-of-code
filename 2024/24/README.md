---
title: "Day 24: Crossed Wires"
url: https://adventofcode.com/2024/day/24
tags: reverse-engineering, bits
---

### Solution
45-bit adder implementing the step:
```
t1 = x ^ y
z = t1 ^ c
t2 = x & y
t3 = t1 & c
c = t2 | t3
```
Tried to visualize using a plain digraph, but wasn't very useful, so switched to [mermaid](https://www.reddit.com/r/adventofcode/comments/1hl8tl4) instead and looked for swaps. That was pretty convoluted to manually parse, too, so I put together a "hybrid" solution where I also implemented the step above for the input connections, manually and gradually **patching the swaps (errors)**, keeping track of them.

It's a messy solution, but it was interactive and fun!

### Usage
```
$ make
```
