INPUT=input

.PHONY: solve
solve: solve.py
	cat $(INPUT) | python3 $^

.PHONY: test
test: test.py solve.py
	pytest -vv $<
