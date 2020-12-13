import sys

class Evaluator:
    valid_instructions = {'nop', 'acc', 'jmp'}

    def __init__(self, code: list):
        self.code = code
        self.ACC  = 0
        self.IP   = 0
        self.__ips = set() # executed instructions

    def __advance(self, delta=1) -> None:
        self.__ips.add(self.IP)
        self.IP += delta

    def run_until_loop(self) -> int:
        while self.IP < len(self.code):
            inst, arg = self.code[self.IP]
            delta = 1 # normally, go to next instruction

            if inst == 'nop':
                pass

            elif inst == 'acc':
                self.ACC += int(arg)

            elif inst == 'jmp':
                delta = int(arg)
                next_ip = self.IP + delta
                if next_ip in self.__ips: # loop found
                    break

            else:
                raise ValueError(f'Invalid instruction {inst} {arg}')

            self.__advance(delta)

        return self.ACC


if __name__ == "__main__":
    with open(sys.argv[1], 'rt') as fp:
        code = [tuple(l.strip().split()) for l in fp.readlines()]

    print(f'Part 1: {Evaluator(code).run_until_loop()}')