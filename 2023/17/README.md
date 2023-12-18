---
title: "Day 17: Clumsy Crucible"
url: https://adventofcode.com/2023/day/17
tags: pathfinding, graphs
---

### Solution
```
Part 1: 936
Part 2: 1157
```

The idea is to use a special state: `(position, orientation, fuel)` and do the search in this space (with vanilla Dijkstra).
- `fuel` is the number of steps we can still take in the same direction as the current cell
- the edge cost is the cell value from the grid

The search implementation is the same for both parts, only the function to get the neighbours of a cell is different:
- part 1: can move <= 3 cells in the same direction; if we're changing direction, the fuel is reset
- part 2: move 4 <= n <= 10 cells in the same direction; the trick here is to yield neighbours checking if at the end of the 4-cell streak we are still in bounds (o/w this streak-starting state is invalid); then, if we're inside a streak, just yield the next move, otherwise the logic is the same as for part 1

I had some bugs after wrapping my state (tuple) in the `Cell` class, because I was not careful about immutability. Should've just used tuples to begin with. [^1]

### Usage
```
$ make
```

[^1]: https://www.reddit.com/r/adventofcode/comments/18ku1d3/2023_day_17_part_1_getting_close_but_somethings/
