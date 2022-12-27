## [Day 18: Operation Order](https://adventofcode.com/2020/day/18)

### general
Parsing parentheses is done by finding the pattern `l(e)p`.
`e` is evaluated recursively, then the result (`e'`) will be used to flatten the expression:
`l(e)p -> le'p`. For example:
```
1 + (2 * 3) + 4
l = 1 +
e = 2 * 3
r = + 4
=> le'p = 1+6+4
```

### same precedence
Having `(\d+)` at the end enforces a `r->l` fold which will be eval `l->r`:
```
5 * 2 + 7 * 3
5 * 2 + 7 | * | 3
5 * 2 | + | 7 | * | 3
5 | * | 2 | + | 7 | * | 3
10 | + | 7 | * 3
17 | * | 3
51
```

### `prec(+) > prec(*)`
It's important that `mul` is parsed first, so that `add` will essentially be "extracted".
In the example below, parsing `mul -> mul` will isolate an `add`:
```
5 * 2 + 7 * 3 + 4 * 6
5 * 2 + 7 * 3 + 4 | * | 6
5 * 2 + 7 | * | 3 + 4 | * | 6
5 | * | 2 + 7 | * | 3 + 4 | * | 6
5 | * | 9 | * | 7 | * | 6
45 | * | 7 | * 6
315 | * | 6
1890
```
