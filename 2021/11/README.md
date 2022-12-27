## [Day 11: Dumbo Octopus](https://adventofcode.com/2021/day/11)

### Solution
- Make use of a [numpy masked array](https://numpy.org/doc/stable/reference/maskedarray.generic.html) to store
energy level (`.data`) and the flag indicating whether the cell has flashed (`.mask`).
- In a simulation step, do the following:
  - for each cell with energy level > 9: increase the energy level of all neighbouring cells
  - if this update leads to new cells with energy level > 9, repeat
  - otherwise stop, and return the 0-filled masked array (i.e. all cells that flashed are filled with 0)
```
Part 1: 1669
Part 2: 351
```

### Usage
```
$ make
```

#### Animation
```
$ make animate
```
Here's an animation of the sample input, until all octopuses flash (flashing cells are marked with red):
<p align="center">
  <img src="./ani.gif">
</p>
