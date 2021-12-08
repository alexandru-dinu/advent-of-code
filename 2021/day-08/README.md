## [Day 8: Seven Segment Search](https://adventofcode.com/2021/day/8)

### Solution
- Decoding, i.e. obtaining a mapping from wires `a-g` to correct display segment indices `0-6`, is done in steps.
- Construct a set of possible wires that each segment can have, then continuously filter the values until each wire
is mapped to exactly one segment.
- One way of doing this is by making use only of the following digits (and their number of **on** segments):
  - 1 (2 seg), 7 (3 seg), 4 (4 seg), and 2,3,5 (5 seg)
- For more details, check the [`decode`](https://github.com/alexandru-dinu/advent-of-code/blob/main/2021/day-08/solve.coco#L40) function.
```
Part 1: 303
Part 2: 961734
```

### Usage
```
$ make
```

### Notes
There is a much simpler and clever solution (see this [Reddit post](https://www.reddit.com/r/adventofcode/comments/rbj87a/2021_day_8_solutions/hnoyy04/)).

Essentially, one can identify the digit by knowing three things:
1. number of segments needed (e.g. `1: 2, 7: 3, 4: 4` etc.)
2. number of segments in common with 4 (which has exactly 4 segments)
3. number of segments in common with 1 (which has exactly 2 segments)

Hence, the following decision tree can be constructed:
```
<number of segments>
    2 -> 1
    3 -> 7
    4 -> 4
    7 -> 8
    5 -> [2,3,5]
        <in common with 4 (4 seg)>
            2 -> 2
            3 -> [3, 5]
                <in common with 1 (2 seg)>
                    1 -> 5
                    2 -> 3
    6 -> [0,6,9]
        <in common with 4 (4 seg)>
            4 -> 9
            3 -> [0, 6]
                <in common with 1 (2 seg)>
                    1 -> 6
                    2 -> 0
```
For instance:
- `8` can be fully identified only by knowing it requires 7 segments.
- `3` can be fully identified by knowing that it requires 5 segments, and that it has 3 segments in common with 4, and 2 segments in common with 1.
