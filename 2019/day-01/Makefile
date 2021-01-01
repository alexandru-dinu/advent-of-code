IN=input/input.txt

.PHONY: run clean

run: Main.class $(IN)
	scala -cp . Main $(IN)
	$(MAKE) clean

Main.class: Main.scala
	scalac $^

clean:
	rm -f *.class
