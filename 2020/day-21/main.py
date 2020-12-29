import sys
import re
from itertools import chain
from collections import defaultdict, Counter
from functools import reduce

def backtrack(pool):
    def _inner(pool, acc):
        if None not in acc.values():
            return True

        for i, ps in pool:
            for p in ps - set(acc.values()):
                acc[i] = p
                if _inner(pool[1:], acc):
                    return True
                acc[i] = None

        return False

    acc = {k: None for k,_ in pool}
    _inner(pool, acc)
    return acc


if __name__ == "__main__":
    with open(sys.argv[1], 'rt') as fp:
        xs = fp.read().strip().split('\n')

    r = re.compile(r'([\w ]+)\(contains (.*)\)')

    xs = [r.match(x).groups() for x in xs]
    xs = [(k.split(), v.split(', ')) for k,v in xs]

    foods = set(chain(*[x[0] for x in xs]))
    ags = set(chain(*[x[1] for x in xs]))

    ag2food = defaultdict(lambda: foods)

    for ag in ags:
        for c_food, c_ags in xs:
            if ag in c_ags:
                ag2food[ag] = ag2food[ag] & set(c_food)

    # Each allergen is found in exactly one ingredient.
    # Each ingredient contains zero or one allergen.
    ag2food = backtrack(sorted(ag2food.items(), key=lambda x: len(x[1])))
    ag_food = set(ag2food.values())

    c = Counter()
    for c_food, _ in xs:
        c.update(set(c_food) - ag_food)

    print(f'Part 1: {sum(c.values())}')

    ss = ','.join([x[1] for x in sorted(ag2food.items(), key=lambda x: x[0])])
    print(f'Part 2: {ss}')
