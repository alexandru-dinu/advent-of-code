---
title: "Day 12: Hill Climbing Algorithm"
url: https://adventofcode.com/2022/day/12
---

### Solution
```
Part 1: 517
Part 2: 512
```
Since the graph is unweighted, we can find the shortest path using BFS.
This works because nodes are explored in increasing distance from the `source` and are "locked-in" once visited,
meaning that once a new node `N` is visited, we have the shortest path from `source` to `N`.

For part 2, where we have to compute the shortest path from all `a` nodes to the `end`, we have two options:
1. initialize the queue with all `a` nodes (implemented)
2. start the search from the `end` to all `a` nodes, then return the min dist to `a` (the `neighbours` function has to be modified for this reversed traversal)

### Usage
```
$ make
```
