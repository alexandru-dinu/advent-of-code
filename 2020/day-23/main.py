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

class Env:
    def __init__(self, xs):
        self.val2node = {}
        self.head = self.__mklist(xs)
        self.min = min(xs)
        self.max = max(xs)
        assert len(self.val2node) == len(xs)

    def __mklist(self, xs: list) -> Node:
        head, *tail = xs
        head = Node(head, is_head=True)
        self.val2node[head.val] = head

        cur = head
        for x in tail:
            node = Node(x)
            self.val2node[x] = node
            cur.next = node
            cur = cur.next
        cur.next = head

        return head

    def take(self, x0: int, n: int):
        out = []
        node = self.val2node[x0]
        for _ in range(n):
            out.append(node.val)
            node = node.next
        return out

    def show(self) -> None:
        cur = self.head
        while cur.next != self.head:
            print(cur, end=' -> ')
            cur = cur.next
        print(cur)

    def step(self):
        def _find_dest(x: Node, xs: Node, tri: list) -> Node:
            # key thing: find max less than x which is not in the picked-up 3 (tri)
            if x.val == 1:
                c = self.max
                while c in tri:
                    c -= 1
                return self.val2node[c]

            c = x.val - 1

            while c in tri:
                c -= 1
                # not found here => it's the max from the rest
                if c < self.min:
                    return self.val2node[self.max]

            return self.val2node[c]


        def _inner(head: Node) -> Node:
            # h_old -> (. -> . -> tri ) -> h_new -> ...
            h_old = head
            tri   = h_old.next.next.next
            h_new = tri.next

            dest = _find_dest(h_old, tri.next,
                tri=self.take(h_old.next.val, n=3))
            tri.next   = dest.next
            dest.next  = h_old.next
            h_old.next = h_new

            h_old.is_head = False
            h_new.is_head = True

            return h_new

        self.head = _inner(self.head)
        return self

    def iterate(self, n: int):
        for _ in trange(n):
            # self.show()
            self.step()
            # input()
        return self


if __name__ == "__main__":
    with open(sys.argv[1], 'rt') as fp:
        xs = list(map(int, fp.read().strip()))

    out = Env(xs).iterate(100).take(x0=1, n=len(xs))[1:]
    print(f'Part 1: {"".join(map(str, out))}')

    xs = [*xs, *range(max(xs) + 1, 1_000_000 + 1)]
    a, b = Env(xs).iterate(10_000_000).take(x0=1, n=3)[1:]
    print(f'Part 2: {a * b}')