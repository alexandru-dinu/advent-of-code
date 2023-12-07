INPUT=input

.PHONY: solve
solve: solve.py
	cat $(INPUT) | python3 $^

.PHONY: test
test: solve.py
	pytest -vv $^

solve.py: solve.coco
	coconut solve.coco
