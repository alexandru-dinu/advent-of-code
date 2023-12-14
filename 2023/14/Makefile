INPUT=input

.PHONY: solve
solve: solve.py
	cat $(INPUT) | python3 $^

solve.py: solve.coco
	coconut solve.coco

.PHONY: test
test: solve.py
	pytest -vv $^
