## [Day 13: Transparent Origami](https://adventofcode.com/2021/day/13)

### Solution
- Represent the grid as a boolean numpy array, then:
    - folding along `y` means `top | flip_up_down(bot)`
    - folding along `x` means `left | flip_left_right(right)`
```
Part 1: 678
Part 2:
████  ██  ████ █  █ █    █  █ ████ ████
█    █  █ █    █  █ █    █  █    █ █
███  █    ███  ████ █    ████   █  ███
█    █    █    █  █ █    █  █  █   █
█    █  █ █    █  █ █    █  █ █    █
████  ██  █    █  █ ████ █  █ ████ █
```

### Usage
```
$ make
```
