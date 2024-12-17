from pathlib import Path
from typing import TextIO

import z3


def parse(fp: TextIO):
    regs, prog = fp.read().split("\n\n")
    A, B, C = map(lambda r: int(r.split(":")[1]), regs.split("\n"))
    prog = [int(x) for x in prog.split(":")[1].strip().split(",")]
    assert len(prog) % 2 == 0

    return A, B, C, prog


def run(A, B, C, prog):
    COMBO = {
        0: lambda: 0,
        1: lambda: 1,
        2: lambda: 2,
        3: lambda: 3,
        4: lambda: A,
        5: lambda: B,
        6: lambda: C,
    }

    out = []

    ip = 0
    while ip < len(prog):
        opc, ope = prog[ip], prog[ip + 1]
        assert 0 <= opc <= 7
        match opc:
            case 0:  # adv
                A //= 2 ** COMBO[ope]()
                ip += 2
            case 1:  # bxl
                B ^= ope
                ip += 2
            case 2:  # bst
                B = COMBO[ope]() % 8
                ip += 2
            case 3:  # jnz
                ip = ope if A != 0 else ip + 2
            case 4:  # bxc
                B ^= C
                ip += 2
            case 5:  # out
                out.append(COMBO[ope]() % 8)
                ip += 2
            case 6:  # bdv
                B = A // (2 ** COMBO[ope]())
                ip += 2
            case 7:  # cdv
                C = A // (2 ** COMBO[ope]())
                ip += 2

    return out


def solve_constraints(tgt):
    s = z3.Solver()

    A = z3.BitVec("A", 64)
    _A = A  # make a copy b/c the original A will get modified

    for y in tgt:
        B = (A % 8) ^ 3
        C = z3.UDiv(A, (1 << B))
        A = z3.UDiv(A, 8)
        B = (B ^ 5) ^ C
        s.add(y == B % 8)

    s.add(A == 0)

    # minimize A
    while s.check() == z3.sat:
        model = s.model()
        s.add(_A < model[_A])

    return model[_A]


def solve(A, B, C, prog):
    p1 = ",".join(str(x) for x in run(A, B, C, prog))
    p2 = solve_constraints(prog)

    return p1, p2


def test_example():
    with open(Path(__file__).parent / "example") as fp:
        assert [4, 6, 3, 5, 6, 3, 5, 2, 1, 0] == run(*parse(fp))

    def solve_constraints_small():
        s = z3.Solver()
        _A = A = z3.BitVec("A", 32)
        for y in [0, 3, 5, 4, 3, 0]:
            A = z3.UDiv(A, 8)
            s.add(y == A % 8)
        s.add(A == 0)
        while s.check() == z3.sat:
            model = s.model()
            s.add(_A < model[_A])
        return model[_A]

    assert 117440 == solve_constraints_small()


def main():
    with open(0) as fp:
        p1, p2 = solve(*parse(fp))

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main()
