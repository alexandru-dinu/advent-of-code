## [Day 7: No Space Left On Device](https://adventofcode.com/2022/day/7)

### Solution
```
Part 1: 1667443
Part 2: 8998590
```
Overkill parsing and tree traversals.

### Upsolving
[Upsolving in Rust](./upsolve.rs), following the neat [structural pattern matching idea](https://www.reddit.com/r/adventofcode/comments/zesk40/comment/iz8fww6).
The gist is that we can just populate a mapping from dir `/foo/bar` to its total size,
while parsing the commands -- making further simplifying assumptions.

### Usage
```
$ make
```
