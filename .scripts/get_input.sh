#!/usr/bin/env bash

set -euo pipefail

YEAR="$1"
DAY="$2"
URL="https://adventofcode.com/$YEAR/day/${DAY##0}/input"

curl -X GET $URL \
    -H "User-Agent: https://github.com/alexandru-dinu/advent-of-code/blob/main/.scripts/get_input.sh" \
    -H "Cookie: session=$AOC_SESSION" \
