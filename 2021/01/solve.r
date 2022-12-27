part1 <- function(xs) {
    sum(diff(xs) > 0)
}

# no need for convolution: just ignore overlapping regions
# i    i+3
# |     |
# A A A |
#   B B B
part2 <- function(xs, window_size=3) {
    count <- 0
    for (i in seq_len(length(xs) - window_size)) {
        if (xs[i + window_size] > xs[i]) {
            count <- count + 1
        }
    }
    count
}

args <- commandArgs(trailingOnly = TRUE)
xs <- scan(args[2])

sprintf("Part 1: %d", part1(xs))
sprintf("Part 2: %d", part2(xs))