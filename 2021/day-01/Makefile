INPUT=input

all: python julia haskell

python: solve.py
	python $^ $(INPUT)

julia: solve.jl
	julia $^ $(INPUT)

haskell: solve.hs
	runhaskell $^ $(INPUT)
