---
title: "Day 15: Warehouse Woes"
url: https://adventofcode.com/2024/day/15
tags: sim
---

### Solution
Simulate the moves.

For part 1 where boxes take 1 cell, they can be easily pushed by the robot simply by swapping the first box with the first empty cell at the end of a run of boxes.

For part 2, since boxes span 2 cells, once a collision with either side of the box is the next move, run a BFS to find the entire ensemble of boxes that needs to be moved, manually tracking `"[" -> "]"` and `"]" <- "[`. If at any point during the search an obstacle is found, the search terminates and no ensemble is returned (no move).
Finally, the ensemble (`p`) is moved through a grid copy to `p + ori`.

The code is unnecessarily convoluted...

### Usage
```
$ make
```
