INPUT=input

solve: solve.py
	python $< --file $(INPUT)

adt: solve.py Solve.hs
	python $< --file $(INPUT) --adt | runhaskell $(word 2,$^)
