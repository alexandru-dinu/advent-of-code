IN=input/input.txt

.PHONY: run clean

Main: Main.hs
	ghc -i../intcode --make -dynamic -o $@ $^

Main.class: Main.scala
	scalac $^

clean:
	rm -f Main *.o *.hi *.class
