import numpy as np


class Vec2(np.ndarray):
    def __new__(cls, x, y):
        return np.array([x, y], dtype=int).view(cls)

    def __eq__(self, other):
        return np.array_equal(self, other)

    def __hash__(self):
        x, y = self
        return hash((x, y))

    def __str__(self):
        return f"({self.x}, {self.y})"

    __repr__ = __str__

    @property
    def x(self):
        return self[0]

    @property
    def y(self):
        return self[1]
