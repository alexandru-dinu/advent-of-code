INPUT=input

py: solve.py
	@cat $(INPUT) | python3 $^

test: test.py solve.py
	pytest --hypothesis-show-statistics $<
