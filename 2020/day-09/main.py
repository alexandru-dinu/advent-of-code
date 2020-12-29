import sys

TARGET = 23278925 # input.txt
# TARGET = 127      # sample.txt

def contiguous_subset_sum(xs: list, target: int) -> list:
    lo, hi = 0, 2

    while hi < len(xs):
        subset = xs[lo:hi]
        c_sum  = sum(subset)

        if c_sum == target:
            return subset
        hi += (c_sum < target)
        lo += (c_sum > target)

    assert False

if __name__ == "__main__":
    with open(sys.argv[1], 'rt') as fp:
        xs = [int(x.strip()) for x in fp.readlines()]

    subset = contiguous_subset_sum(xs, TARGET)
    print(f'Part 2: {min(subset) + max(subset)}')