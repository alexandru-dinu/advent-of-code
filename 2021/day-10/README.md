## [Day 10: Syntax Scoring](https://adventofcode.com/2021/day/10)

### Solution
- Parse the string using a stack and maintain a `status, expected, found, column, stack` context.
- For **corrupt** strings: accumulate score given by `found` char.
- For **incomplete** strings: dump the contents of remaining stack (reversed) and match with the corresponding chars.
```
Part 1: 166191
Part 2: 1152088313
```

### Usage
```
$ make stack
```

### Notes
- [`grammar.py`](./grammar.py) uses [Lark](https://lark-parser.readthedocs.io/en/latest/) to define the grammar
and construct a parser:
```python
grammar = """
    start: s

    s: round s | square s | brace s | angle s |
    round:  "(" s ")"
    square: "[" s "]"
    brace:  "{" s "}"
    angle:  "<" s ">"
"""
parser = Lark(grammar)
```
- During parsing, there are two key exceptions to look for:
  - [`UnexpectedCharacters`](https://lark-parser.readthedocs.io/en/latest/classes.html#lark.exceptions.UnexpectedCharacters): for **corrupt** strings
  - [`UnexpectedEOF`](https://lark-parser.readthedocs.io/en/latest/classes.html#lark.exceptions.UnexpectedEOF): for **incomplete** strings
- _(Naive)_ Finding the remaining chars (for incomplete strings) is done as follows:
  - choose the closing char when encountering a `UnexpectedEOF` exception
  - add it to the string
  - repeat until the string is correctly parsed
  - finally return the added suffix
