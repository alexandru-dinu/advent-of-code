## [Day 21: Monkey Math](https://adventofcode.com/2022/day/21)

### Solution
```
Part 1: 155708040358220
Part 2: 3342154812537
```
Parse expressions into terminals and non-terminals, then traverse and evaluate the expression tree.

For part 2, a `l - r = 0` equation is constructed, with the unknown `humn`. Solving is done using sympy.

### Usage
```
$ make
```
