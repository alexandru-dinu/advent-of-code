INPUT=input

all: python julia haskell R

python: solve.py
	python $^ $(INPUT)

julia: solve.jl
	julia $^ $(INPUT)

haskell: solve.hs
	runhaskell $^ $(INPUT)

R: solve.r
	Rscript $^ --args $(INPUT)
