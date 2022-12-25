import heapq
import multiprocessing as mp
import re
from functools import partial

import numpy as np
from tqdm import tqdm

ORE, CLAY, OBSIDIAN, GEODE = range(4)


def parse_line(line: str) -> tuple[int, np.ndarray]:
    # for simplicity, make each robot cost all 4 resources (default 0, if not needed)
    # row: robots, col: resources
    costs = np.zeros((4, 4), dtype=int)
    (
        bid,
        costs[ORE, ORE],
        costs[CLAY, ORE],
        costs[OBSIDIAN, ORE],
        costs[OBSIDIAN, CLAY],
        costs[GEODE, ORE],
        costs[GEODE, OBSIDIAN],
    ) = map(int, re.findall(r"(\d+)", line))
    return bid, costs


def heuristic(state):
    # higher is better

    # total resources + total robots
    return sum(state[:-1])

    # geode-oriented
    # return state[ORE] + state[OBSIDIAN] + state[GEODE] + state[4 + GEODE]


def score_single(blueprint, time) -> int:
    bid, costs = blueprint

    #    v--res---v  v--bot---v  t
    s = (0, 0, 0, 0, 1, 0, 0, 0, time)

    pq = []
    heapq.heappush(pq, (0, s))  # (priority, state)
    seen = set()
    best = 0

    while pq:
        _, cur = heapq.heappop(pq)
        o, c, O, g, ro, rc, rO, rg, t = cur
        best = max(best, g)

        if t == 0:
            continue

        # pruning surplus resources
        o = min(o, (t - 1) * costs[:, ORE].max())
        c = min(c, (t - 1) * costs[OBSIDIAN, CLAY])
        O = min(O, (t - 1) * costs[GEODE, OBSIDIAN])
        cur = (o, c, O, g, ro, rc, rO, rg, t)

        if cur in seen:
            continue
        seen.add(cur)

        # if len(seen) % 1_000_000 == 0:
        #     print(
        #         f"[{mp.current_process().pid}] {len(pq)=:,d}, {len(seen)=:,d}, {best=}, {cur=}"
        #     )

        # in one time step we can do one of the following:

        # 1/ collect items with current robots
        nxt = (o + ro, c + rc, O + rO, g + rg, ro, rc, rO, rg, t - 1)
        if nxt not in seen:
            heapq.heappush(pq, (-heuristic(nxt), nxt))

        # 2/ build new robots

        # pruning: if there's not enough time to build a geode robot, skip
        if t < 3 and rg == 0 and o < costs[GEODE, ORE] and O < costs[GEODE, OBSIDIAN]:
            continue

        # pruning:
        #   if bot <kind> costs n <res>, then shouldn't have more than n <res> bots
        #   b/c I can only build 1 bot/t anyway

        # ORE
        if o >= costs[ORE, ORE] and ro < costs[:, ORE].max():
            nxt = (
                o + ro - costs[ORE, ORE],
                c + rc,
                O + rO,
                g + rg,
                ro + 1,
                rc,
                rO,
                rg,
                t - 1,
            )
            if nxt not in seen:
                heapq.heappush(pq, (-heuristic(nxt), nxt))

        # CLAY
        if o >= costs[CLAY, ORE] and rc < costs[OBSIDIAN, CLAY]:
            nxt = (
                o + ro - costs[CLAY, ORE],
                c + rc,
                O + rO,
                g + rg,
                ro,
                rc + 1,
                rO,
                rg,
                t - 1,
            )
            if nxt not in seen:
                heapq.heappush(pq, (-heuristic(nxt), nxt))

        # OBSIDIAN
        if (
            o >= costs[OBSIDIAN, ORE]
            and c >= costs[OBSIDIAN, CLAY]
            and rO < costs[GEODE, OBSIDIAN]
        ):
            nxt = (
                o + ro - costs[OBSIDIAN, ORE],
                c + rc - costs[OBSIDIAN, CLAY],
                O + rO,
                g + rg,
                ro,
                rc,
                rO + 1,
                rg,
                t - 1,
            )
            if nxt not in seen:
                heapq.heappush(pq, (-heuristic(nxt), nxt))

        # GEODE
        if o >= costs[GEODE, ORE] and O >= costs[GEODE, OBSIDIAN]:
            nxt = (
                o + ro - costs[GEODE, ORE],
                c + rc,
                O + rO - costs[GEODE, OBSIDIAN],
                g + rg,
                ro,
                rc,
                rO,
                rg + 1,
                t - 1,
            )
            if nxt not in seen:
                heapq.heappush(pq, (-heuristic(nxt), nxt))

    return bid, best


def get_scores(blueprints, time):
    scores = {}

    work_fn = partial(score_single, time=time)

    with mp.Pool(processes=min(len(blueprints), mp.cpu_count())) as pool:
        with tqdm(total=len(blueprints), ascii=True) as pbar:
            for bid, best in pool.imap_unordered(work_fn, blueprints):
                pbar.update(1)
                scores[bid] = best

    return scores


def main():
    with open(0) as fp:
        blueprints = [parse_line(x.strip()) for x in fp]

    scores = get_scores(blueprints, time=24)
    print("Part 1:", sum(k * v for k, v in scores.items()))

    scores = get_scores(blueprints[:3], time=32)
    print("Part 2:", np.prod(list(scores.values())))


if __name__ == "__main__":
    main()
