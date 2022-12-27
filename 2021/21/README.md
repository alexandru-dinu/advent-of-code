## [Day 21: Dirac Dice](https://adventofcode.com/2021/day/21)

### Solution
```
Part 1: 428736
Part 2: 57328067654557
```

#### Deterministic 100-sided die
- We can define the following recurrence relations:
```
P1(0) = start1
P1(t) = P1(t-1) + (1+2+3) + (t-1) * (6+6+6)
      = P1(t-1) + 18(t-1) + 6
      = start1 + 9t^2 - 3t

P2(0) = start2
P2(t) = P2(t-1) + (4+5+6) + (t-1) * (6+6+6)
      = P2(t-1) + 18(t-1) + 15
      = start2 + 9t^2 + 6t
```
- The score for player `i` up to move `t` is:
```
Si(t) = sum_{j=1..t} (Pi(j) - 1) % 10 + 1
```

#### Dirac 3-sided die
- First, we represent the state of the game as a tuple:
```
state = (
    pos of current player, score of current player,
    pos of   other player, score of   other player
)
```
- Then, define a function `:: state -> (int, int)` that computes `(wins p1, wins p2)` given a game state.
  - This allows memoization, i.e. caching `(state, wins)` pair to avoid unnecessary computation.
- A game turn is as follows:
  - given the sum of the 3 die rolls (`3**3` new branches), current player's position and score is updated
  - a new state is constructed, where the current player here becomes the previous other player
  - a recursive call with this new state is performed, updating the win-count for the parent accordingly
- Finally, `max(wins1, wins2)` is returned, given the initial state.

### Universe weighting
- [Reddit discussion](https://www.reddit.com/r/adventofcode/comments/rlalpm/2021_day_21_where_are_the_best_starting_positions/hpgbwzp/) regarding universe weighting and probability calculation.
- Let's consider a simpler example, where there are 2 rolls per turn and a 2-sided die (or coin):
```
      L1    L2
*
|
+- *
|  |
|  +- w1
|  |
|  `- w1
|
`- *
   |
   +- w1
   |
   `- *
      |
      +- *
      |  |
      |  +- w2
      |  |
      |  `- w2
      |
      `- *
         |
         +- w2
         |
         `- w2
```
- Here, player 1 wins 3 times and player 4 wins 4 times. The naive calculation `w1 / (w1 + w2)` yields `3/7 ~ 0.428`.
- However, this disregards the fact that all universes of a given depth have equal weight.
- To account for this, we can "extend" the winning sub-trees `w1` downwards, so as to obtain a full tree.
- This now gives `3 * 4` more wins for player 1, the probability now is `3*4 / (3*4 + 4) = 3/4`.
- And this makes sense, if you look at the first level `L1`: player 1 wins 3 times and player 2 wins a full sub-tree,
therefore we account a full fraction of a win `= 1` at this level, so we get `3/(3+1) = 3/4`.
- Additionally, instead of down-propagating winning sub-trees, we can up-propagate discounted wins.
  - a win at level `l` is worth `1 / (27**l)`, which gets propagated upwards

#### Probability that P1 wins given `(start_1, start_2)`
|    |      1 |      2 |      3 |      4 |      5 |      6 |      7 |      8 |      9 |     10 |
|---:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|
|  1 | 0.6757 | 0.6324 | 0.6172 | 0.6574 | 0.7227 | 0.7787 | 0.7911 | 0.7776 | 0.7519 | 0.7211 |
|  2 | 0.7231 | 0.6830 | 0.6681 | 0.7045 | 0.7650 | 0.8174 | 0.8293 | 0.8167 | 0.7927 | 0.7646 |
|  3 | 0.7335 | 0.6953 | 0.6810 | 0.7156 | 0.7733 | 0.8235 | 0.8349 | 0.8228 | 0.7998 | 0.7730 |
|  4 | 0.6816 | 0.6416 | 0.6275 | 0.6649 | 0.7254 | 0.7773 | 0.7887 | 0.7760 | 0.7520 | 0.7235 |
|  5 | 0.6080 | 0.5638 | 0.5495 | 0.5920 | 0.6590 | 0.7155 | 0.7274 | 0.7135 | 0.6871 | 0.6551 |
|  6 | 0.5500 | 0.5017 | 0.4866 | 0.5340 | 0.6074 | 0.6687 | 0.6812 | 0.6662 | 0.6375 | 0.6022 |
|  7 | 0.5406 | 0.4908 | 0.4753 | 0.5242 | 0.5998 | 0.6629 | 0.6758 | 0.6604 | 0.6309 | 0.5945 |
|  8 | 0.5559 | 0.5066 | 0.4912 | 0.5393 | 0.6140 | 0.6765 | 0.6894 | 0.6741 | 0.6450 | 0.6091 |
|  9 | 0.5844 | 0.5366 | 0.5213 | 0.5675 | 0.6399 | 0.7007 | 0.7134 | 0.6986 | 0.6704 | 0.6357 |
| 10 | 0.6213 | 0.5753 | 0.5600 | 0.6039 | 0.6736 | 0.7325 | 0.7451 | 0.7308 | 0.7035 | 0.6703 |

### Usage
```
$ make
```
