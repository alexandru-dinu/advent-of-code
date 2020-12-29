import sys
from typing import List
from collections import Counter


def group_yes_count(xs: List[str]) -> int:
    num_persons = len(xs)

    yes_counter = Counter()
    for x in xs:
        yes_counter.update(x)

    # how many questions were answered "yes" by all persons in the group
    return sum([1 for count in yes_counter.values() if count == num_persons])

if __name__ == "__main__":
    with open(sys.argv[1], 'rt') as fp:
        data = [x.split() for x in fp.read().split('\n\n')]

    print(f'Part 1: {sum(map(lambda x: len(set("".join(x))), data))}')
    print(f'Part 2: {sum(map(group_yes_count, data))}')