## [Day 11: Monkey in the Middle](https://adventofcode.com/2022/day/11)

### Solution
```
Part 1: 56595
Part 2: 15693274740
```
Just follow the instructions.
For part 2 (where div 3 is not longer allowed), keep passed values `mod d1 * d2 * ... * dN`, where each `di` is a test value.
Doing this keeps the values manageable and preserves divisibility by each `d1, ..., dN`, since:
```
((x mod (d1 * ... * dN)) mod dj) == (x mod dj), j = 1..N
```

### Usage
```
$ make
```
