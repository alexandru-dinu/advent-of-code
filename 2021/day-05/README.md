## [Day 5: Hydrothermal Venture](https://adventofcode.com/2021/day/5)

Solved in the [Coconut](http://coconut-lang.org/) language.

### Solution
- Represent the grid as a numpy array.
- Use the signs of `Δy` and `Δx` to determine the line direction (i.e. -1, 0, 1).
- Use `[ys, xs]` indexing to mark all points from the line.
- Count all grid entries `>= 2`.
```
Part 1: 4826
Part 2: 16793
```

### Usage
```
$ make
```
