---
title: "Day 19: Linen Layout"
url: https://adventofcode.com/2024/day/19
tags: search, memoization
---

### Solution
Memoized search (DFS).

Once we exhaust the string, we populate the cache and if we have to solve the same suffix, reuse the result from the cache.
That is, we may encounter the same suffix in different calls: `(s1 | prefix1)` and `(s2 | prefix2)` can both ask for the same remaining suffix to solve, so reuse from cache.

Example input:
```
ab, cd, abc, d, xyz

abcdxyz
```
There are 2 ways:
```
abcdxyz
  ab|cd (xyz)
  abc|d (xyz)
```
both calls will require the result of `xyz` suffix:
```
+++ caching args=('',); mem[args]=1
+++ caching args=('xyz',); mem[args]=1
+++ caching args=('cdxyz',); mem[args]=1
>>> reusing args=('xyz',); mem[args]=1
+++ caching args=('dxyz',); mem[args]=1
+++ caching args=('abcdxyz',); mem[args]=2
```

### Usage
```
$ make
```
