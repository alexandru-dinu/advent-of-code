## [Day 20: Jurassic Jigsaw](https://adventofcode.com/2020/day/20)

- Assembly image by doing: candidates -> valid -> recurse with the rest of the tiles.
- Find monsters by performing a convolution between the grid and the monster mask.
  - The number of monsters is then computed by counting how many values in the conv output are equal to the number of `#` in the monster mask.
