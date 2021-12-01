```
              0, 1, 2, 3, 4, 5, ...
                    [--j--]  i
    e.g. xs: [0, 1, 4, 5, 6, 7, ...]
   num_ways: [1, 1, 1, 1, 2, 4, ...]

there are 4 pos. of reaching from 4 to 7:
1) 4 (+1) 5 (+1) 6 (+1) 7
2) 4 (+1) 5 (+2) 7
3) 4 (+2) 6 (+1) 7
4) 4 (+3) 7

thus: num_ways[5] = num_ways[4] + num_ways[3] + num_ways[2]

NOTE: looks like a (masked) Tribonacci(len(xs))
also similar to Fibonacci staircase (where you can climb 1 or 2 stairs at once)
here you can "move" 3 steps at once, so just count the number of ways to get to those steps:
4 -> 7 (+3)
5 -> 7 (+2)
6 -> 7 (+1)
```