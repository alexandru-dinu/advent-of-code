INPUT=input

.PHONY: solve
solve: solve.py
	python3 $^ $(INPUT)

.PHONY: test
test: solve.py
	pytest -vv $^
