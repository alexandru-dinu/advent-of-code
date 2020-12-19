import sys
import re


r_wse = re.compile(r'\s*')
r_num = re.compile(r'^(\d+)$')
r_par = re.compile(r'^(.*)\(([\d\+\*]+)\)(.*)$') # l|(p)|r

def parse_with(r_add, r_mul) -> callable:

    def _parseval(expr: str) -> int:
        # 0. remove whitespace
        expr = re.sub(r_wse, '', expr)

        # 1. flatten parens by evaluating + replacing them with the result
        if (m := r_par.match(expr)):
            lhs, parens, rhs = m.groups()
            parens = _parseval(parens)
            return _parseval(''.join([lhs, str(parens), rhs]))

        # 2. multiplication
        if (m := r_mul.match(expr)):
            lhs, rhs = map(_parseval, m.groups())
            return lhs * rhs

        # 3. addition
        if (m := r_add.match(expr)):
            lhs, rhs = map(_parseval, m.groups())
            return lhs + rhs

        # 4. number
        if (m := r_num.match(expr)):
            return int(m.group(1))

        raise ValueError(f'Cannot parse: {expr}')

    return _parseval


if __name__ == "__main__":
    with open(sys.argv[1], 'rt') as fp:
        xs = fp.read().strip().split('\n')

    # same precedence
    parser = parse_with(
        r_add=re.compile(r'^(.+)\+(\d+)$'),
        r_mul=re.compile(r'^(.+)\*(\d+)$'))
    print(f'Part 1: {sum(map(parser, xs))}')

    # prec(+) > prec(*)
    parser = parse_with(
        r_add=re.compile(r'^(.+)\+(.+)$'),
        r_mul=re.compile(r'^(.+)\*(.+)$'))
    print(f'Part 2: {sum(map(parser, xs))}')