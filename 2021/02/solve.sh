#!/bin/bash

part1='
{
    switch ($1) {
        case "down":
            d += $2; break;
        case "up":
            d -= $2; break;
        case "forward":
            h += $2; break;
    }
}
END {print h * d}
'

part2='
{
    switch ($1) {
        case "down":
            a += $2; break;
        case "up":
            a -= $2; break;
        case "forward":
            h += $2; d += a * $2; break;
    }
}
END {print h * d}
'

if [ $# -lt 1 ]; then
    echo "Usage: $0 <input>"
    exit 1
fi

input_file="$1"

echo Part 1: $(awk "$part1" "$input_file")
echo Part 2: $(awk "$part2" "$input_file")