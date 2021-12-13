## [Day 12: Passage Pathing](https://adventofcode.com/2021/day/12)

### Solution
- Maintain a `can_visit[node]` count telling how many times `node` can be visited (e.g. `inf` for big caves, `1` for small caves).
- Part 1 is just DFS.
- (_A bit suboptimal_) For part 2, for each small caves, allow them to be visited twice, then apply DFS. Finally accumulate all the unique paths.
```
Part 1: 4241
Part 2: 122134
```

### Usage
```
$ make
```
