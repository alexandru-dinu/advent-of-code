from __future__ import annotations

import sys

from common import parse_input

if __name__ == "__main__":
    with open(sys.argv[1], "rt") as fp:
        rule_dict, msg_set = parse_input(fp.read().strip())

    import IPython

    IPython.embed(using=False)
