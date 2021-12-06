INPUT=input

all: coconut haskell

coconut: solve.coco
	coconut --run --no-write $^ --args $(INPUT)

haskell: solve.hs
	runhaskell $^ $(INPUT)

naive: naive.py
	python $^ --file $(INPUT) --plot
