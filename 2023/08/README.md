---
title: "Day 8: Haunted Wasteland"
url: https://adventofcode.com/2023/day/8
tags: graphs
---

### Solution
```
Part 1: 12361
Part 2: 18215611419223
```

Part 1 is a simple traversal.

For part 2, I noticed that from each starting node (ending in `A`), all destination nodes (ending in `Z`) will loop back to themselves after the same number of steps, i.e.:
```
         ┌────┐
    n    ▼    │n
A───────►Z────┘
```
This means that for all starting nodes to reach the destination nodes, we must compute the lowest common multiple of the individual path lengths.

### Usage
```
$ make
```
