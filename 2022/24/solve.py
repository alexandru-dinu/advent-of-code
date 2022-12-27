import heapq
from collections import defaultdict
from copy import deepcopy

BLIZZARD = set("^v<>")


class PQ:
    # Annoying wrapper around heapq to allow complex numbers
    def __init__(self):
        self.pq = []

    def __len__(self):
        return len(self.pq)

    def push(self, state, prio):
        time, z = state
        heapq.heappush(self.pq, (prio, (time, z.imag, z.real)))

    def pop(self):
        prio, (time, y, x) = heapq.heappop(self.pq)
        # return (time, complex(x, y)), prio
        return time, complex(x, y)


class Grid:
    def __init__(self, h, w, pos):
        self.h = h
        self.w = w
        self.pos = pos

    def in_bounds(self, z):
        return 0 <= z.real < self.w and 0 <= z.imag < self.h

    def show(self):
        for y in range(self.h):
            for x in range(self.w):
                c = self.pos[complex(x, y)]
                if len(c) == 0:
                    c = "."
                elif len(c) == 1:
                    c = c[0]
                else:
                    c = len(c)
                print(c, end="")
            print()


def move_blizzard(grid):
    new = deepcopy(grid)

    for z in grid.pos:
        for c in grid.pos[z]:
            if c not in BLIZZARD:
                continue

            new.pos[z].remove(c)
            if not new.pos[z]:  # don't keep empty lists
                del new.pos[z]

            x, y = z.real, z.imag
            x2, y2 = x, y

            match c:
                case "^":
                    y2 = y - 1 if y >= 2 else grid.h - 2
                case "v":
                    y2 = y + 1 if y < grid.h - 2 else 1
                case "<":
                    x2 = x - 1 if x >= 2 else grid.w - 2
                case ">":
                    x2 = x + 1 if x < grid.w - 2 else 1

            new.pos[complex(x2, y2)].append(c)

    return new


def manh(z1, z2):
    return abs(z1.real - z2.real) + abs(z1.imag - z2.imag)


def grid_gen(grid):
    while True:
        yield grid
        grid = move_blizzard(grid)


def find_path(grid, start, end, t_start=0) -> int:
    # returns smallest number of steps from start to end
    pos = start

    G = grid_gen(grid)
    grids = [next(G) for _ in range(t_start + 1)]

    # state is (time, pos)
    state = (t_start, pos)
    pq = PQ()
    pq.push(state, prio=0)
    came_from = {}
    cost_so_far = {state: 0}

    k = 0

    while len(pq) > 0:
        k += 1
        cur_state = pq.pop()
        time, cur = cur_state

        # if k % 10000 == 0:
        #     print(
        #         f"> exploring {cur} at time {time}, dist: {manh(cur, end)}, grids: {len(grids)}"
        #     )

        if cur == end:
            best_time = cost_so_far[cur_state]
            # print("found path", time, cur, cost_so_far[cur_state])

            # reconstruct path
            path = [cur_state]
            while cur_state in came_from:
                cur_state = came_from[cur_state]
                path.insert(0, cur_state)

            for t, p in path:
                # print(t, p)
                assert grids[t].pos[p] == []

            return best_time - t_start

        # update time step and add new grids if necessary
        time += 1
        if time >= len(grids):
            grids.extend([next(G) for _ in range(time - len(grids) + 1)])

        # get the grid at current time
        grid_t = grids[time]

        # sanity check: ensure that the current position is empty
        assert grids[time - 1].pos[cur] == []

        # next: Stay, R, L, D, U
        # operates on the grid at time t (i.e. after the increment above)
        for dz in [0, 1, -1, 1j, -1j]:
            nxt = cur + dz

            # print(f">> {time} {cur} -> {nxt}; {grid_t.pos[nxt]}")

            if not grid_t.in_bounds(nxt):
                continue

            # nxt pos is blocked
            if len(grid_t.pos[nxt]) > 0:
                # print(f"\t{nxt} is blocked by {grid_t.pos[nxt]}")
                continue

            # these nxt states are only valid for grid_t
            nxt_state = (time, nxt)
            prio = time + manh(nxt, end)

            # "new cost" is time
            if nxt_state not in cost_so_far or time < cost_so_far[nxt_state]:
                cost_so_far[nxt_state] = time
                pq.push(nxt_state, prio=prio)
                came_from[nxt_state] = cur_state
                # print(f"\t{nxt_state} added")

    assert False, "no path found"


def animate(grid):
    import os

    os.system("clear")

    G = grid_gen(grid)

    xs = [next(G) for _ in range(10)]

    k = 0
    while k < 10:
        print(k)
        xs[k].show()
        i = int(input())
        k += i
        os.system("clear")


def main():
    with open(0) as fp:
        pos = defaultdict(list)
        h, w = 0, 0
        for y, line in enumerate(fp):
            h += 1
            for x, c in enumerate(line.strip()):
                w = max(w, x + 1)
                if c != ".":  # no need to store empty cells
                    pos[complex(x, y)].append(c)

    grid = Grid(h, w, pos)
    # animate(grid)

    start = complex(1, 0)
    end = complex(grid.w - 2, grid.h - 1)

    print("Part 1:", find_path(grid, start, end))

    t0 = find_path(grid, start, end, t_start=0)
    t1 = find_path(grid, end, start, t_start=t0)
    t2 = find_path(grid, start, end, t_start=t0 + t1)
    print("Part 2:", t0 + t1 + t2)


if __name__ == "__main__":
    main()
