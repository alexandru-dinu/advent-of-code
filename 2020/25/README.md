## [Day 25: Combo Breaker](https://adventofcode.com/2020/day/25)

```
find c, d (loop sizes) s.t.:
    7 ** c ≡ pkc (mod m)
    7 ** d ≡ pkd (mod m)
    pkc ** d % m == pkd ** c % m

exp = discrete_log(res, base, mod)
```

|              | explicit modexp | [pow](https://docs.python.org/3/library/functions.html#pow)       |
| ---          | ---             | ---       |
| brute-force  | 105.6s          | 41.5s     |
| [discrete log](https://docs.sympy.org/latest/modules/ntheory.html#sympy.ntheory.residue_ntheory.discrete_log) | **0.27s**       | **0.27s** |
