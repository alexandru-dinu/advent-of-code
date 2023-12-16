---
title: "Day 12: Hot Springs"
url: https://adventofcode.com/2023/day/12
tags: strings, memoization
---

### Solution
```
Part 1: 7032
Part 2: 1493340882140
```

Terminology:
```
???.### 1,1,3
^       ^---- targets
\--- pattern
```

Part 1 can be solve naively by checking all possible combinations.

For part 2 (some hints from [^1]), the logic is the following:
- at any point, we can encounter `.`, `#`, `?`
- if we get `.`, then we can just skip it, keeping the target
- if we get `#`, then we need to check if the current `#` run equals to the first target
    - we can just exit early (with 0) if this constraint is not satisfied
    - otherwise, we can check for exhausting the current targets: if this is the final run of `#` and there are no more targets, we found a new combination
    - finally, we can continue with the next target and the rest of the pattern, provided if we're separated by `?` or `.` (o/w, just exit with 0)
- if we get `?`, then we do both `? -> .` and `? -> #` and sum the outcomes
    - when "changing" the `?` to `.`, we are actually doing the same recursive call as when just encountering a `.` (**this can be memoized**)

### Usage
```
$ make
```

[^1]: https://www.reddit.com/r/adventofcode/comments/18hbbxe/2023_day_12python_stepbystep_tutorial_with_bonus/
