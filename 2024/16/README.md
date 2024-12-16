---
title: "Day 16: Reindeer Maze"
url: https://adventofcode.com/2024/day/16
tags: pathfinding
---

### Solution
Part 1: DFS w/ cost tracking. Initially I stored only the position in a `cost` structure, but this wrongly omits better paths through the same `pos`, but with a different orientation, so the state is both `pos, ori`. Also, this `cost` structure acts as a `visited` structure b/c we won't advance to a new cell unless the new cost to it is worth it (e.g. moving back to a visited cell `x` will unnecessarily increase `x`'s computed cost, so it won't be visited).

### Usage
```
$ make
```
