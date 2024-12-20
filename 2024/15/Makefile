INPUT=input

.PHONY: solve
solve: solve.py
	python3 $^

.PHONY: test
test: solve.py
	pytest -vv $^
