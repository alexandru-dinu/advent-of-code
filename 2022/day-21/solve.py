import re

from sympy import Symbol, solve
from sympy.parsing.sympy_parser import parse_expr
from sympy.solvers import solve


def parse(exprs: list[str]) -> tuple[dict, dict]:
    ts = {}  # terminals
    nonts = {}  # non-terminals

    for expr in exprs:
        lhs, rhs = expr.split(": ")

        try:
            ts[lhs] = int(rhs)
        except ValueError:
            x, op, y = re.match("([a-z]+)\s([+-/*])\s([a-z]+)", rhs).groups()
            nonts[lhs] = (op, x, y)

    return ts, nonts


def eval_with(ts, nonts) -> callable:
    def _inner(key) -> float:
        if key in ts:
            return ts[key]

        op, l, r = nonts[key]
        return eval(f"lambda x, y: x {op} y")(_inner(l), _inner(r))

    return _inner


def eq_with(ts, nonts, unk: str) -> callable:
    def _inner(key) -> str:
        if key in ts:
            return key if key == unk else str(ts[key])

        op, l, r = nonts[key]
        return f"({_inner(l)} {op} {_inner(r)})"

    return _inner


def main():
    with open(0) as fp:
        exprs = [x.strip() for x in fp]

    ts, nonts = parse(exprs)

    evaluator = eval_with(ts, nonts)
    print("Part 1:", int(evaluator(key="root")))

    unk = "humn"
    eq = eq_with(ts, nonts, unk)
    _, l, r = nonts["root"]
    l = parse_expr(eq(l))
    r = parse_expr(eq(r))
    print("Part 2:", solve(l - r, Symbol(unk)).pop())


if __name__ == "__main__":
    main()
