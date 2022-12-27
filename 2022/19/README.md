---
title: "Day 19: Not Enough Minerals"
url: https://adventofcode.com/2022/day/19
tags: optimization, state-space-search, pruning, heuristics
---

### Solution
```
Part 1: 1613
Part 2: 46816
```
State-space search. At each time step we can either collect new resources with the available robots or spend the resources to build new robots. However, the total number of states will become intractable, so we have to employ some heuristics and pruning:
- priority queue with the heuristic: "total resources & robots", i.e. "richer" states get explored first
- pruning surplus resources: if we currently have more (ore, clay, obsidian) than we could possible spend, throw the surplus away
- if there's not enough time to build a geode robot, skip
- if a bot of `<kind>` costs `n` `<res>`, we don't need more than `n <res>` bots, because we can only build 1 bot/t anyway

Another improvement (not implemented) can include skipping time steps in-between robot building.

Blueprints are processed individually in a `multiprocessing.Pool`. Not extremely efficient, however: ~3m45s for part 2.

Some hints from [@jonathanpaulson][1] and [@mrphlip][2].

[1]: https://github.com/jonathanpaulson
[2]: https://github.com/mrphlip

### Usage
```
$ make
```
