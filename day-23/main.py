import sys
from tqdm import trange


class Node:
    def __init__(self, val, is_head=False):
        self.val = val
        self.is_head = is_head
        self.next = None

    def __eq__(self, other):
        return self.val == other.val

    def __lt__(self, other):
        return self.val < other.val

    def __str__(self):
        if self.is_head:
            return f'({self.val})'
        return f'{self.val}'

    __repr__ = __str__

def show(h):
    cur = h
    while cur.next != h:
        print(cur, end=' -> ')
        cur = cur.next
    print(cur, end=f' => {h}')


def mklist(xs):
    head, *tail = xs

    head = Node(head, is_head=True)

    cur = head
    for x in tail:
        cur.next = Node(x)
        cur = cur.next
    cur.next = head

    return head


def find_dest(h, sl):
    # TODO: extremely slow...
    c = sl

    max_ = Node(-1)
    while not c.is_head:
        max_ = max(max_, c)
        c = c.next

    if h.val == 1:
        return max_

    for i in range(h.val - 1, 0, -1):
        c = sl

        while not c.is_head:
            if i == c.val:
                return c
            c = c.next

    return max_


def step(head):
    h1 = head
    tri = h1.next.next.next
    h2 = tri.next
    d = find_dest(h1, tri.next)
    tri.next = d.next
    d.next = h1.next
    h1.next = h2
    h1.is_head = False
    h2.is_head = True

    return h2


def iterate(xs, n):
    head = mklist(xs)
    for _ in trange(n):
        head = step(head)
    return head


def take(head, x0, n):
    c = head
    while c.val != x0:
        c = c.next
    c = c.next

    out = ''
    for _ in range(n):
        out += str(c.val)
        c = c.next
    return out


if __name__ == "__main__":
    with open(sys.argv[1], 'rt') as fp:
        xs = list(map(int, fp.read().strip()))

    head = iterate(xs, n=100)
    print(f'Part 1: {take(head, x0=1, n=len(xs) - 1)}')

    xs = [*xs, *range(max(xs) + 1, 1_000_000 + 1)]
    head = iterate(xs, n=10_000_000)
    print(f'Part 2: {take(head, x0=1, n=2)}')
