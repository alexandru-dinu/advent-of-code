import sys
from typing import List, NewType, Tuple

InstructionType = NewType("InstructionType", Tuple[str, str])  # e.g. jmp +2
CodeType = NewType("CodeType", List[InstructionType])


class Evaluator:
    valid_instructions = {"nop", "acc", "jmp"}

    def __init__(self, code: CodeType):
        self.code = code
        self.ACC = 0
        self.IP = 0
        self.__ips = set()  # executed instructions

    def __advance(self, delta=1) -> None:
        self.__ips.add(self.IP)
        self.IP += delta

    def run(self) -> Tuple[bool, int]:
        while self.IP < len(self.code):
            inst, arg = self.code[self.IP]
            delta = 1  # normally, go to next instruction

            if inst == "nop":
                pass

            elif inst == "acc":
                self.ACC += int(arg)

            elif inst == "jmp":
                delta = int(arg)
                next_ip = self.IP + delta
                if next_ip in self.__ips:  # loop found
                    return False, self.ACC  # False = early termination

            else:
                raise ValueError(f"Invalid instruction {inst} {arg}")

            self.__advance(delta)

        return True, self.ACC


def repair(code: CodeType) -> CodeType:
    def flip(loc: InstructionType) -> InstructionType:
        inst, arg = loc

        if inst == "jmp":
            return "nop", arg
        if inst == "nop":
            return "jmp", arg

        raise ValueError(f"Invalid instruction {inst} {arg}")

    for i, (inst, arg) in enumerate(code):
        if inst == "acc":
            continue

        code[i] = flip(code[i])
        status, acc = Evaluator(code).run()
        if status == True:  # loop was fixed
            return code
        code[i] = flip(code[i])

    # unreachable
    assert False


if __name__ == "__main__":
    with open(sys.argv[1], "rt") as fp:
        code: CodeType = [tuple(l.strip().split()) for l in fp.readlines()]

    status, acc = Evaluator(code).run()
    assert status == False  # loop has to be found
    print(f"Part 1: {acc}")

    status, acc = Evaluator(repair(code)).run()
    assert status == True  # loop was fixed
    print(f"Part 2: {acc}")
