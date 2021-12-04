## [Day 4: Giant Squid](https://adventofcode.com/2021/day/4)

### Solution
- Representing the boards as [numpy masked arrays](https://numpy.org/doc/stable/reference/maskedarray.generic.html) allows for easy
number marking and checking for winning boards.
- The difference between parts reduces to finding the score of the first and last winning board.
```
Part 1: 38913
Part 2: 16836
```

### Usage
```
$ make
```
