---
title: "Day 10: Pipe Maze"
url: https://adventofcode.com/2023/day/10
tags: graphs, maths, 2d, geometry
---

### Solution
```
Part 1: 7173
Part 2: 291
```

Part 1 is DFS with parent reconstruction, yielding the loop (with length `L`). The max distance on this loop from the start (as given by BFS) is therefore `L / 2`.

Part 2 was tricky. I couldn't get the [even-odd rule](https://en.wikipedia.org/wiki/Point_in_polygon#Ray_casting_algorithm) to work, because of the edge cases when the ray cast overlaps with the polygon boundary (the loop). I looked for some hints and learned [^1] about [Shoelace formula](https://en.wikipedia.org/wiki/Shoelace_formula) for polygon area and [Pick's theorem](https://en.wikipedia.org/wiki/Pick%27s_theorem) relating:
```
area = #(inside) + #(boundary)/2 - 1
=> #(inside) = area - #(boundary)/2 + 1
```

Should definitely upsolve and brush-up my computer graphics algorithms.

### Usage
```
$ make
```

[^1]: https://www.reddit.com/r/adventofcode/comments/18evyu9/comment/kcqmhwk
