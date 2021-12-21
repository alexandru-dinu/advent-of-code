## [Day 21: Dirac Dice](https://adventofcode.com/2021/day/21)

### Solution

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

```
Part 1: 428736
Part 2: 57328067654557
```

### Usage
```
$ make
```
