INPUT=input

hs: solve.hs
	runhaskell $^ $(INPUT)

py: solve.py
	cat $(INPUT) | python3 $^
