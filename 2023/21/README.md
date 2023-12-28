---
title: "Day 21: Step Counter"
url: https://adventofcode.com/2023/day/21
tags: 2d, maths
---

### Solution
```
Part 1: 3809
Part 2: 629720570456311
```

Part 1 is BFS, returning the number of nodes in the queue after `n` steps.

Part 2 was tricky and I relied on some hints from the [subreddit][1] about constructing a mapping from the number of steps to the number of reachable cells. Moreover, the number of steps for part 2 is `26501365 = 202300 * 131 + 65`, with `131` being the size of the grid. So I fit a polynomial of degree 2 (given 3 data points `(x, y)`):
```
y = P(x)
x = 0 => 0 * 131 + 65 = 65  => y =  3911 steps
x = 1 => 1 * 131 + 65 = 196 => y = 34786 steps
x = 2 => 2 * 131 + 65 = 327 => y = 96435 steps
```
Then the answer for part 2 is `P(202300)`.

### Usage
```
$ make
```

[1]: https://www.reddit.com/r/adventofcode/comments/18nevo3/2023_day_21_solutions/
