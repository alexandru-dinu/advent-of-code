.PHONY: new
new:
	mkdir -p $(YEAR)
	cp -r .template/ $(YEAR)/$(DAY)/
	source ./.env && ./.scripts/get_input.sh $(YEAR) $(DAY) > $(YEAR)/$(DAY)/input

.PHONY: badges
badges: README.md
	source ./.env && mdup -i $< && sed -i '/`/d' $<

.PHONY: clean
clean:
	find . -name "__pycache__" -print0 | xargs -0 rm -rf
	find . -name "__coconut_cache__" -print0 | xargs -0 rm -rf
	find . -name ".hypothesis" -print0 | xargs -0 rm -rf
	find . -name ".pytest_cache" -print0 | xargs -0 rm -rf
	find . -name ".ipynb_checkpoints" -print0 | xargs -0 rm -rf
	find . -name ".mypy_cache" -print0 | xargs -0 rm -rf

SRC_PY := $(shell find ./ -name "*.py")
.PHONY: format-py
format-py:
	@autoflake --remove-all-unused-imports -i $(SRC_PY)
	@isort $(SRC_PY)
	@black $(SRC_PY)

# find the parent dirs of the Makefiles with a `test` rule
TEST_MK := $(shell find . -mindepth 2 -not -path '*/\.*' -name "Makefile" -exec grep -l "test:" {} \; | xargs -n 1 dirname)
.PHONY: test
test:
	@for mk in $(TEST_MK); do \
		$(MAKE) -C $$mk test || exit 1; \
	done

.PHONY: run-year
run-year:
	@for d in $(shell seq -w 1 25); do \
		$(MAKE) -C ./$(YEAR)/$$d || exit 1; \
	done
