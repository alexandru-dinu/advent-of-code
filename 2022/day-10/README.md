---
title: "Day 10: Cathode-Ray Tube"
url: https://adventofcode.com/2022/day/10
---

### Solution
```
Part 1: 12880
Part 2:
####  ##    ##  ##  ###    ## ###  ####
#    #  #    # #  # #  #    # #  # #
###  #       # #  # #  #    # #  # ###
#    #       # #### ###     # ###  #
#    #  # #  # #  # #    #  # # #  #
#     ##   ##  #  # #     ##  #  # ####
```
- part 1: yield `(clk, X)` values while executing the instructions
    - (alt. solution): introduce dummy 0s, then do a cumulative sum to get the `X` values for each `clk`
```
noop   -> 0
addx x -> [0, x] # requires 2 cycles
```
- part 2: draw each line checking if the current pixel being drawn is part of the sprite

### Usage
```
$ make
```
