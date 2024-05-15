XDG_DATA_HOME            ?= $(HOME)/.local/share
BASH_COMPLETION_USER_DIR ?= $(XDG_DATA_HOME)/bash-completion/completions

build:
	stack build
.PHONY: build

install: build
	stack install
	install -d $(BASH_COMPLETION_USER_DIR)
	install -m 0644 compleat_setup $(BASH_COMPLETION_USER_DIR)
.PHONY: install

install-fish: build
	stack install
	install -d ~/.config/fish
	install -m 0644 compleat_setup.fish ~/.config/fish
.PHONY: install-fish
