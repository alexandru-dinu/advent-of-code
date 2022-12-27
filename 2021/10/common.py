def corrupt_score(c: str) -> int:
    return {
        ")": 3,
        "]": 57,
        "}": 1197,
        ">": 25137,
    }.get(c)


def incomplete_score(xs: str) -> int:
    # fmt: off
    s = (
        xs.replace(")", "1")
          .replace("]", "2")
          .replace("}", "3")
          .replace(">", "4")
    )
    # fmt: on
    return int(s, 5)
