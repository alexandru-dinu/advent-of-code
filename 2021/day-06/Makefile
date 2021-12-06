INPUT=input

all: coconut

coconut: solve.coco
	coconut --run --no-write $^ --args $(INPUT)

naive: naive.py
	python $^ --file $(INPUT) --plot
