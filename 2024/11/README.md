---
title: "Day 11: Plutonian Pebbles"
url: https://adventofcode.com/2024/day/11
tags: sim
---

### Solution
The main idea is to use a counter per node per level and apply the update only per unique nodes, updating the counts as level progresses.
For example, given `2 2`, we only process the update rule for the unique node `2` and propagate downwards the counts for all new nodes coming out of it.

Can use a graph for the whole representation and progress, but it's not mandatory.

### Usage
```
$ make
```
