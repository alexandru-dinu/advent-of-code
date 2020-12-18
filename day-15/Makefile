CC=g++
CFLAGS=-std=c++11 -O1 -Werror -Wall -Wextra

.PHONY: clean

run: main
	./main input/input.txt

main: main.cpp
	$(CC) $(CFLAGS) $^ -o $@

clean:
	rm -f main