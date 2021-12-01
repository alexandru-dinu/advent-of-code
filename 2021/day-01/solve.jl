function part1(xs)
    sum(diff(xs) .> 0)
end

function valid_conv(xs, w)
    n = length(xs)
    m = length(w)
    rw = reverse(w)

    [sum(xs[i:i+m-1] .* rw) for i=1:(n-m+1)]
end

function part2(xs)
    w = repeat(1:1, 3)
    part1(valid_conv(xs, w))
end

xs = open(ARGS[1]) do fp
    map(x -> parse(Int, x), split(read(fp, String)))
end

println("Part 1: ", part1(xs))
println("Part 2: ", part2(xs))
