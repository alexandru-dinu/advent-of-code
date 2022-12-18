## [Day 17: Pyroclastic Flow](https://adventofcode.com/2022/day/17)

### Solution
```
Part 1: 3069
Part 2: 1523167155404
```
Representing shapes as sets of complex numbers for easy 2D manip and collision checking.

For part 2, after some extra steps (`e`), we will encounter a cycle of period `tau`. This means that
we can compute `height(T) = extra + dHeight * (T - e) / tau`, where `dHeight` is the height increment b/w 2 periods.

#### Follow-up
We can also solve part 2 by inferring the relevant information (period start and size) given a list of observations `(t, height(t))`.

See [function_period_detection.ipynb](./function_period_detection.ipynb).

### Usage
```
$ make
```
