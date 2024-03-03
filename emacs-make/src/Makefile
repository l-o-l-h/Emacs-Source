# Makefile -- Install New Version of Emacs -- -*- mode: make; -*-
# Time-stamp: <2024-01-18 10:26:18 minilolh>

DEST = ${HOME}/.local/share/emacs
EMACS-DIR = $(DEST)/emacs
EMACS-SRC = https://git.savannah.gnu.org/git/emacs.git

dest:	$(DEST)

$(DEST):
	mkdir -vp $(DEST)

emacs-dir: $(EMACS-DIR)

$(EMACS-DIR):
	clone

clone:	| dest
	cd $(DEST); git clone --depth 1 $(EMACS-SRC);

configure: | emacs-dir
	# start here
