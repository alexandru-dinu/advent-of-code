## [Day 12: Passage Pathing](https://adventofcode.com/2021/day/12)

### Solution

#### Upsolving
- Using a "can visit twice" boolean flag for small caves (see this [Reddit post](https://www.reddit.com/r/adventofcode/comments/rehj2r/2021_day_12_solutions/ho7ufss/)),
part 2 is greatly simplified.
- The general idea is the following:
  - Big nodes don't count as visited.
  - Small nodes are either:
    - visited once, passing the flag further.
    - visited twice (i.e. during the first visit, they are not accounted for).

#### Old solution
- Maintain a `can_visit[node]` count telling how many times `node` can be visited (e.g. `inf` for big caves, `1` for small caves).
- Part 1 is just DFS.
- (_A bit suboptimal_) For part 2, for each small caves, allow them to be visited twice, then apply DFS.
Finally accumulate all the unique paths.
```
Part 1: 4241
Part 2: 122134
```

### Usage
```
$ make
```
