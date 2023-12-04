---
title: "Day 4: Scratchcards"
url: https://adventofcode.com/2023/day/4
tags: trees, counting
---

### Solution
```
Part 1: 18619
Part 2: 8063216
```

Part 1 is just following the rule.

For part 2, we maintain a `Counter` for each card and do a traversal from last to first, updating
card `i` with info from cards `i+1, ..., i+n` (depending on card `i`'s points). This essentially builds multiple sub-trees.
Finally, we aggregate the counters from all cards.

Example: if card 3 has 2 points (winning cards 4 and 5), we update card 3 counter with counts from cards 4 and 5. Then we continue with card 2, which also has 2 points (winning cards 3 and 4).
```
         2      /----- 3
...    /   \    |    /   \    ...
      [3]  [4]  |  [4]   [5]
       ^        |
       \---------
```

### Usage
```
$ make
```
