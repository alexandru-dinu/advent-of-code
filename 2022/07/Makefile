INPUT=input

.PHONY: py rs rs-test

py: solve.py
	@cat $(INPUT) | python3 $^

rs: upsolve.rs
	@rustc $^ -o $@
	@cat $(INPUT) | ./$@
	@rm $@

rs-test: upsolve.rs
	@rustc $^ --test -o $@
	@./$@
	@rm $@

