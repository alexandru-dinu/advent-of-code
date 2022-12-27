## [Day 16: Packet Decoder](https://adventofcode.com/2021/day/16)

### Solution
- Use a monadic parser to construct the `Packet` ADT: `values` or `operators`.
- Summing version and evaluating the packets is done via a simple traversal.
- The interesting part was parsing the operator packets:
  - For length type id 1, `n` is the number of sub-packets => apply packet parser `n` times.
  - For length type id 0, `n` is the length of the sub-string containing the sub-packets =>
  apply packet parser to this sub-string ([StackOverflow](https://stackoverflow.com/q/70386003)).
```
Part 1: 883
Part 2: 1675198555015
```

### Usage
```
$ make
```
