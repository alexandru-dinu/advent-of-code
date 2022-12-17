## [Day 16: Proboscidea Volcanium](https://adventofcode.com/2022/day/16)

### Solution
```
Part 1: 1906
Part 2: 2548
```
Implemented 2 methods:
- part 1: traversal
    - visit all reachable nodes with positive flow and continue recursively with the remaining nodes
    - essentially, construct _subsequences_ of the openable valves keeping track of the max possible flow
- part 2: dynamic programming (a bit slow, >1m)
    - the state is given by `(time budget, current node, current valve states: 1 = openable)`
    - in one step (`t-1 -> t`) we can do two actions: open a valve or move to a neighboring node
        - the current state is updated with the value of the max flow possible given these actions
    - since openable valves are represented bitwise, we can construct 2 disjoint sets of valves as:
        - `x` (for human) and `(N - 1) ^ x` for the elephant
        - check for all `x` in `[0, N/2)`, where `N = 2 ** openable_valves`
- hints from [AoC subreddit](https://www.reddit.com/r/adventofcode/comments/zn6k1l/2022_day_16_solutions)

### Usage
```
$ make
```
