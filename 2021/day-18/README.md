## [Day 18: Snailfish](https://adventofcode.com/2021/day/18)

### Solution
- The effects of the **split** and **explode** operations are performed by recursively assembling a new tree.
- **split**
  - the new node `R (x >= 10) -> P (R floor(x/2)) (R ceil(x/2))` is propagated upwards.
- **explode**
  - part of the assembling, carry two `(x, y)` values from the `P (R x) (R y)` node that exploded.
  - `y` is left-added to the right sub-tree; the new carry becomes `(x, 0)`.
  - `x` is right-added to the left sub-tree, the new carry becomes `(0, y)`.
  - this ensures that `x`, `y` are added to the closest regular values to the node that exploded.
  - the new sub-trees are propagated upwards.
- Idea from [AoC sub-reddit](https://www.reddit.com/r/adventofcode/comments/rizw2c/2021_day_18_solutions/hp0o2hl/).
```
Part 1: 4176
Part 2: 4633
```

#### Old attempt
- Tried an in-place solution where the side-effects were propagated using `.parent` links, but it was a bit cumbersome
and harder to debug.

### Usage
```
$ make
```
