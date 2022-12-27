## [Day 14: Extended Polymerization](https://adventofcode.com/2021/day/14)

### Solution
- First, convert input to numeric format `0..n` as it's easier to work it.
- Then, perform a n-step simulation for each `ij` pair in the input pattern, finally returning the counter for this pair.
- Finally, aggregate the counters for each pair, accounting for double counting the middle elements.
- The simulation makes use of the insertion and pair frequency matrices.
- The insertion matrix represents the rewritten rules, e.g. `I[i,j] = k` corresponds to `ij -> k` (`ikj`).
- The pair frequency matrix is given w.r.t. a particular step; its non-zero values `F[i,j]` count the number
of occurrences of the pair `ij`.
- A simulation step is as follows:
  - For each non-zero `f = F[i,j]` (i.e. pair `ij` appears `f` times) get the inserted element `k = I[i,j]`.
  - This insertion produces:
    - `f` new `ik` and `kj` pairs => update frequency matrix
    - `f` more instances of `k` => update element counter
```
Part 1: 2967
Part 2: 3692219987038
```

### Usage
```
$ make
```
