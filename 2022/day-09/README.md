## [Day 9: Rope Bridge](https://adventofcode.com/2022/day/9)

### Solution
```
Part 1: 6081
Part 1: 2487
```
Represent the rope as a list of positions: `[head, *knots]`.
First, the head is moved, then all knots are updated using parent link as reference, i.e. `(rope[i-1], rope[i])`.

### Usage
```
$ make
```
