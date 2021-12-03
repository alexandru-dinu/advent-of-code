## [Day 3: Binary Diagnostic](https://adventofcode.com/2021/day/3)

Solved in the [Coconut](http://coconut-lang.org/) language.

### Solution
- **Part 1:** use [`scipy.stats.mode`](https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.mode.html) to find the most and least common bits for each column
- **Part 2:** recursively shrink the input array (by searching) until only one item remains
```
Part 1: 1025636
Part 2: 793873
```

### Usage
```
$ make
```
