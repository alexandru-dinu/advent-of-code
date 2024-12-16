---
title: "Day 16: Reindeer Maze"
url: https://adventofcode.com/2024/day/16
tags: pathfinding
---

### Solution
Part 1: DFS w/ cost tracking. Initially I stored only the position in a `cost` structure, but this wrongly omits better paths through the same `pos`, but with a different orientation, so the state is both `pos, ori`. Also, this `cost` structure acts as a `visited` structure b/c we won't advance to a new node unless the new cost to it is worth it (e.g. moving back to a visited node `x` will unnecessarily increase `x`'s computed cost, so it won't be visited).

Part 2: Once destination is reached, yield the current path and its cost. The caller will sort all these paths by cost ascending and return both the best cost and all nodes part of all paths with the best cost.

*Edit post-solving:* Could've used a priority queue instead of a vanilla stack and instead of just yielding all paths, we can reset a `best_paths: list[list]` collection every time `best_cost` is updated and keep appending paths to it every time the same `cost == best_cost` is encountered.

### Usage
```
$ make
```
