import sys
import re
from typing import List

class Validator:
    def __init__(self):
        pass

    def validate_byr(x: str) -> bool:
        return 1920 <= int(x) <= 2002

    def validate_iyr(x: str) -> bool:
        return 2010 <= int(x) <= 2020

    def validate_eyr(x: str) -> bool:
        return 2020 <= int(x) <= 2030

    def validate_hcl(x: str) -> bool:
        return re.match(r'^#([0-9a-f]{6})$', x) is not None

    def validate_ecl(x: str) -> bool:
        return x in {'amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'}

    def validate_pid(x: str) -> bool:
        return len(x) == 9

    def validate_hgt(x: str) -> bool:
        match = re.match(r'([0-9]+)(in|cm)', x)
        if match is None:
            return False

        h, scale = match.groups()

        if scale == 'cm':
            return 150 <= int(h) <= 193
        if scale == 'in':
            return 59 <= int(h) <= 76

        return False

    def required_keys() -> set:
        vs = [x for x in dir(Validator) if x.startswith('validate_')]
        vs = [x.split('_')[1] for x in vs] # TODO: nicer?
        return set(vs)

def get_passport(xs: List[str]) -> dict:
    return dict([x.split(':') for x in xs])

def is_valid1(passport: dict) -> bool:
    return len(Validator.required_keys() - passport.keys()) == 0

def is_valid2(passport: dict) -> bool:
    for k in Validator.required_keys():
        func = getattr(Validator, f'validate_{k}')
        if k not in passport or not func(passport[k]):
            return False
    return True


if __name__ == "__main__":
    with open(sys.argv[1], 'rt') as fp:
        # [['key:value']]
        data = [get_passport(x.split()) for x in fp.read().split('\n\n')]

    print(f'Part 1: {sum(map(is_valid1, data))}')
    print(f'Part 2: {sum(map(is_valid2, data))}')