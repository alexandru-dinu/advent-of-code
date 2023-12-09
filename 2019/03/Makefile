INPUT=input

run: Main.hs $(INPUT)
	runhaskell $< $(INPUT)

format: Main.hs
	hindent $^

lint: Main.hs
	hlint $^
