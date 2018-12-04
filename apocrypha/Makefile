all:
	stack build

release:
	stack install --flag apocrypha:release

profile:
	stack build --flag apocrypha:release --profile
	@echo
	@echo run 'stack exec -- <program> +RTS -p'
