IN=input

.PHONY: run

run: Main $(IN)
	./$^ $(IN)

Main: Main.hs
	ghc --make -dynamic -o $@ $^

clean:
	rm Main *.hi *.o
