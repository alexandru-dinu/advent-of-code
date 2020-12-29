import sys

sys.setrecursionlimit(10 * sys.getrecursionlimit())


def play1(p1, p2):
    if not p1 or not p2:
        return p1 or p2 # get non-empty

    h1, *t1 = p1
    h2, *t2 = p2

    if h1 > h2:
        return play1([*t1, h1, h2], t2)
    else:
        return play1(t1, [*t2, h2, h1])


def play2(p1, p2):

    def _inner(p1, p2, mem):
        state = hex(hash((tuple(p1), tuple(p2))))
        if state in mem:
            return 1, p1

        mem.add(state)

        if not p1:
            return 2, p2
        if not p2:
            return 1, p1

        h1, *t1 = p1
        h2, *t2 = p2

        w = None

        if len(t1) >= h1 and len(t2) >= h2:
            w, _ = _inner(t1[:h1], t2[:h2], mem=set())

        if w == 1 or (w is None and h1 > h2):
            return _inner([*t1, h1, h2], t2, mem)

        if w == 2 or (w is None and h2 > h1):
            return _inner(t1, [*t2, h2, h1], mem)

    _, p = _inner(p1, p2, mem=set())
    return p



def compute_score(win):
    return sum([i*c for i,c in zip(range(1, len(win)+1), win[::-1])])


if __name__ == "__main__":
    with open(sys.argv[1], 'rt') as fp:
        p1, p2 = fp.read().strip().split('\n\n')
        p1 = [int(c) for c in p1.split('\n')[1:]]
        p2 = [int(c) for c in p2.split('\n')[1:]]

    win = play1(p1, p2)
    print(f'Part 1: {compute_score(win)}')

    win = play2(p1, p2)
    print(f'Part 2: {compute_score(win)}')
