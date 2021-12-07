## [Day 7: The Treachery of Whales](https://adventofcode.com/2021/day/7)

### Solution
First, we have two score functions:
- part 1: `f(x) = sum_{p in pos} |p - x|`
- part 2: `f(x) = sum_{p in pos} gauss(|p - x|)`, where `gauss(n) = 0.5 * n * (n + 1)`

A straightforward solution is to use binary search to find the min value of the score functions (convex).

Another solution is to directly compute the minimizing `x` values (see [this answer](https://math.stackexchange.com/a/1024462)).
A calculation can be found [here](https://raw.githubusercontent.com/alexandru-dinu/advent-of-code/main/2021/day-07/notes/solution.pdf).

```
Part 1: 347449
Part 2: 98039527
```

### Usage
```
$ make
```
