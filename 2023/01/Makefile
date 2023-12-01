INPUT=input

.PHONY: solve
solve: solve.c
	@gcc -o $@ -Wall -Wextra -Werror $^
	@cat $(INPUT) | ./$@
	@rm $@

.PHONY: format
format: solve.c
	clang-format -i $^
