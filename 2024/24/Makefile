INPUT=input

.PHONY: solve
solve: solve.py
	@python3 $^

out.pdf: out.mermaid
	mmdc -i $^ -o $@ -p puppeteer-config.json -c config.json -w 2000 -H 2000
