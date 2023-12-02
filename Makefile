.PHONY: badges
badges:
	source ./.env && mdup -i README.md

.PHONY: clean
clean:
	find . -name "__pycache__" -print0 | xargs -0 rm -rf
	find . -name ".hypothesis" -print0 | xargs -0 rm -rf
	find . -name ".pytest_cache" -print0 | xargs -0 rm -rf
	find . -name ".ipynb_checkpoints" -print0 | xargs -0 rm -rf
	find . -name ".mypy_cache" -print0 | xargs -0 rm -rf

SRC_PY := $(shell find ./ -name "*.py")
.PHONY: format-py
format-py:
	autoflake --remove-all-unused-imports -i $(SRC_PY)
	isort $(SRC_PY)
	black $(SRC_PY)

.PHONY: test
test:
	find . -name "*.py" \
		-exec grep -l "def test_" {} \; \
		-exec pytest -vv --hypothesis-show-statistics {} \;

