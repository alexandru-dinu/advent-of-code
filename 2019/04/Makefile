.PHONY: run

run: Main
	./$^

Main: Main.hs
	ghc --make -dynamic -o $@ $^

clean:
	rm Main *.hi *.o
