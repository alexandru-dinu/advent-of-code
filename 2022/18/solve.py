from collections import deque
from dataclasses import dataclass
from itertools import product

import matplotlib.pyplot as plt
import numpy as np


@dataclass
class Cube:
    x: int
    y: int
    z: int

    def __hash__(self):
        return hash((self.x, self.y, self.z))

    def __iter__(self):
        yield self.x
        yield self.y
        yield self.z

    def adjacent(self) -> set["Cube"]:
        return {
            Cube(self.x + dx, self.y + dy, self.z + dz)
            for dx, dy, dz in product([-1, 0, 1], repeat=3)
            if sum(map(abs, (dx, dy, dz))) == 1
        }


@dataclass
class Droplet:
    cubes: set[Cube]

    @property
    def bounds(self) -> tuple:
        if not hasattr(self, "_bounds"):
            min_x = min(c.x for c in self.cubes)
            max_x = max(c.x for c in self.cubes) + 1
            min_y = min(c.y for c in self.cubes)
            max_y = max(c.y for c in self.cubes) + 1
            min_z = min(c.z for c in self.cubes)
            max_z = max(c.z for c in self.cubes) + 1
            self._bounds = (min_x, max_x), (min_y, max_y), (min_z, max_z)

        return self._bounds

    def draw(self):
        def _draw_single(ax, dx, dy, dz):
            vertices = np.array(
                [
                    (0, 0, 0),
                    (0, 0, 1),
                    (0, 1, 0),
                    (0, 1, 1),
                    (1, 0, 0),
                    (1, 0, 1),
                    (1, 1, 0),
                    (1, 1, 1),
                ]
            )

            vertices[:, 0] += dx
            vertices[:, 1] += dy
            vertices[:, 2] += dz

            edges = [
                (0, 1),
                (0, 2),
                (0, 4),
                (1, 3),
                (1, 5),
                (2, 3),
                (2, 6),
                (3, 7),
                (4, 5),
                (4, 6),
                (5, 7),
                (6, 7),
            ]

            for edge in edges:
                ax.plot(vertices[edge, 0], vertices[edge, 1], vertices[edge, 2], c="k")

        fig = plt.figure(dpi=200)
        ax = fig.add_subplot(111, projection="3d")
        for cube in self.cubes:
            _draw_single(ax, *cube)
        ax.set_xlabel("x")
        ax.set_ylabel("y")
        ax.set_zlabel("z")
        ax.set_aspect("equal")
        plt.show()


def in_bounds(cube: Cube, bounds: tuple) -> bool:
    (min_x, max_x), (min_y, max_y), (min_z, max_z) = bounds
    return (
        min_x <= cube.x <= max_x
        and min_y <= cube.y <= max_y
        and min_z <= cube.z <= max_z
    )


def count_open_faces(droplet: Droplet) -> int:
    arr = np.array(sorted(map(tuple, droplet.cubes)))
    assert arr.shape == (len(arr), 3)

    # pairwise distance matrix
    dist = np.triu(np.abs(arr[:, np.newaxis, :] - arr[np.newaxis, :, :]).sum(axis=-1))

    # each connected pair blocks 2 faces
    return 6 * len(arr) - 2 * (dist == 1).sum()


def count_open_faces_alt(droplet: Droplet) -> int:
    # alternative implementation
    return sum(len(cube.adjacent() - droplet.cubes) for cube in droplet.cubes)


def fill_air_pockets(droplet: Droplet) -> Droplet:
    (min_x, max_x), (min_y, max_y), (min_z, max_z) = droplet.bounds

    all_cubes = {
        Cube(x, y, z)
        for x, y, z in product(
            range(min_x, max_x + 1), range(min_y, max_y + 1), range(min_z, max_z + 1)
        )
    }

    # reachable by water
    reachable = set()
    q = deque([Cube(min_x, min_y, min_z)])
    while q:
        c = q.popleft()
        if c in reachable:
            continue
        reachable.add(c)
        q.extend([adj for adj in (c.adjacent() - droplet.cubes) & all_cubes])

    # (bounding box) - (reachable by water) = (droplet cubes) + (trapped air)
    return Droplet(cubes=all_cubes - reachable)


def main():
    with open(0) as fp:
        droplet = Droplet(
            cubes={Cube(*tuple(map(int, x.strip().split(",")))) for x in fp}
        )

    surface_area = count_open_faces(droplet)
    assert surface_area == count_open_faces_alt(droplet)
    print("Part 1:", surface_area)

    d_filled = fill_air_pockets(droplet)
    surface_area = count_open_faces(d_filled)
    assert surface_area == count_open_faces_alt(d_filled)
    print("Part 2:", surface_area)


if __name__ == "__main__":
    main()
