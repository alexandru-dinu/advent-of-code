## [Day 6: Lanternfish](https://adventofcode.com/2021/day/6)

### Solution
- Maintain a counter `cnt[k] == number of fish with timer == k (k in {0,1,...,8})`.
- For each initial fish in the population, simulate `n` steps, then observe the count of the final population.
- Finally, sum the observations from each initial fish.
```
Part 1: 353274
Part 2: 1609314870967
```

### Usage
```
$ make
```

### Notes
- [`naive.py`](./naive.py) implements a straightforward simulator maintaining a numpy array containing fish timers.
- Simulating 256 days in this manner is infeasible due to large memory usage.
- Use this approach to construct a simple linear regression model to observe the exponential growth.

#### Example
See `python naive.py --help` for usage.

```
$ python naive.py --file input --days 80

Part 1: 353274
Init pop: 300
Learned freq of doubling: 7.78914
pred(80)  = 370607.31070
pred(256) = 2348864597963.58740
```
