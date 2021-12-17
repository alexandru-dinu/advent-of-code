INPUT=input

solve: Solve.hs Parser.hs
	cat $(INPUT) | python hex2bin.py | runhaskell $<
