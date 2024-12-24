---
title: "Day 21: Keypad Conundrum"
url: https://adventofcode.com/2024/day/21
tags: search, memoization
---

### Solution
There can be N > 1 paths from A to B in a grid T (all shortest, same length).
However, in grid T+1, to resolve these N paths we can have different best paths.

So among the paths which are seemingly identical in grid T, we have to choose the optimal one from grid T+1 perspective.

Example. In grid 1, there are 4 best paths, all w/ the same length:
```
3   7
^A  ^^<<A
^A  <<^^A
^A  ^<^<A
^A  <^<^A
```
Now, to resolve these paths in grid 2, `^A` is always: `<A>A`, but for the 4 different choices for 7, it's better to choose a path starting with `^` as it's closer to `A` (current position in grid 2) than `<`, so:
```
^^<<A   <AAv<AA
^<^<A   <Av<A>^Av<A
```

Navigating the space using e.g. DFS where a state is a path

(path, d) --> (path', d+1)

---

[Original attempt](https://www.reddit.com/r/adventofcode/comments/1hlkrxm/2024_day_21_part_1_help_needed_with_closetofinal/) was not successful, so followed [these hints](https://www.reddit.com/r/adventofcode/comments/1hjx0x4/2024_day_21_quick_tutorial_to_solve_part_2_in/).

Essentially, we start with a helper function to get all valid, non-zigzag paths (h-first, w-first) that solve a sequence given a pad (numpad, dirpad).

Given a code to solve at depth d, we break it down into its subsequences (separated by A) -- so to solve `vA>A` we have to solve both `[vA, >A]` -- then find the optimal path (by going in-depth) for each subsequence, accounting for multiple potential ways of solving it, e.g. `v` can go both `v<A` or `<vA` -- finally adding the optimal length to the total.

The function is memoized by code & depth.

### Usage
```
$ make
```
