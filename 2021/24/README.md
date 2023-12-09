---
title: "Day 24: Arithmetic Logic Unit"
url: https://adventofcode.com/2021/day/24
tags: reverse-engineering, maths
---

### Solution
```
Part 1: 99999795919456
Part 2: 45311191516111
```

Clearly bruteforce is impossible, as there are 9**14 total options. Instead, we can inspect the input and isolate the `inp w` instructions. We see that there are 14 **sub-programs** (each starting with `inp w`), all consisting of 18 instructions.

We further notice that the sub-programs are very similar -- they differ only at 3 instructions.
All unique instr. for the differing indices (0-based):
```
 3: div z [1, 26]
 4: add x [...]
14: add y [...]
```

After analysing the structure of a sub-program, we can translate it roughly to:
```python
def subprog(w, z=0):
    # z is always >= 0
    x = z % 26 + i1
    z //= i2
    if x != w:
        z *= 26
        z += w + i3
    return z
```

This computation looks like manipulating `z` by adding values in base 26.
Specifically, the code pushes a value (`w + i3`) inside the `if`-block and it pops it when doing `z //= 26`. [^1]

We further notice that `z` is divided by either 1 (no-op) or 26 (right-shift) and that there are 7 of each.
Therefore, for each `z //= 1` we push a value and for each `z //= 26` we pop a value. Since `#(push) == #(pop)`, we will end up with `z == 0`.

The condition for push is `x != w` and the condition for pop is `x == w` when `z` is divided by 26.
We can then match the pushed value (when `i2 == 1`), i.e. `w + i3`, to the popped value (when `i2 == 26`), i.e. `z % 26 + i1`.
When we push a value, `x` will always be `> 9`, so the condition `x != w` will always be true.

The last pushed value is `z % 26`, so the condition becomes:
```
x = z % 26 + i1
x = last_pushed + i1
x - i1 = w_prev + i3      # we previously pushed w + i3 when we entered `x != w`
w_cur - i1 = w_prev + i3  # since we want `x == w`
```
We can solve this by matching the push indices (where `i2 == 1`) to the corresponding pop indices (where `i2 == 26`).

### Usage
```
$ make
```

[^1]: push/pop hint from: https://www.reddit.com/r/adventofcode/comments/rnejv5/comment/hps5hgw
