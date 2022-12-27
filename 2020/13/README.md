## [Day 13: Shuttle Search](https://adventofcode.com/2020/day/13)

```
[ 7, 13,  x,  x, 59, x, 31, 19]
  |   |           |      |   |
  t  t+1         t+4    t+6 t+7

t   = multiple of 7
t+1 = multiple of 13
t+4 = multiple of 59
t+6 = multiple of 31
t+7 = multiple of 19

t = 7  * k1     => t ≡  0 (mod  7)
t = 13 * k2 - 1 => t ≡ 12 (mod 13)
t = 59 * k3 - 4 => t ≡ 55 (mod 59)
t = 31 * k4 - 6 => t ≡ 25 (mod 31)
t = 19 * k5 - 7 => t ≡ 12 (mod 19)

All moduli are pairwise coprime. We can apply CRT.
```
