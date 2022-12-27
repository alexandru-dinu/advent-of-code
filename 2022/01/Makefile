INPUT=input

py: solve.py
	python3 $^ $(INPUT)

test: test.py solve.py
	pytest --hypothesis-show-statistics -v $<

hs: solve.hs
	runhaskell $^ $(INPUT)
