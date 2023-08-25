all:
	stack build

release:
	stack install --flag apocrypha:release

.PHONY: test
test:
	stack test

profile:
	stack build --flag apocrypha:release --profile
	@echo
	@echo run 'stack exec -- <program> +RTS -p'
