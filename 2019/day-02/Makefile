IN=input/input.txt

.PHONY: run clean

Main.class: Main.scala
	scalac $^

Main: Main.hs
	ghc --make -dynamic -o $@ $^

clean:
	rm -f Main *.o *.hi *.class
