INPUT=input

py: solve.py
	python3 $^ $(INPUT)

hs: solve.hs
	runhaskell $^ $(INPUT)

test: test.py solve.py
	pytest --hypothesis-show-statistics -v $<
