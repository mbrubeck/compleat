build:
	stack build
.PHONY: build

install: build
	stack install
	install -d ~/.bash_completion.d
	install -m 0644 compleat_setup ~/.bash_completion.d
.PHONY: install
