NAME = nq

help:
	@echo 'Usage: make [target]'
	@echo
	@echo 'Development Targets:'
	@echo '  all      Build NQ and run source code checks.'
	@echo '  nq       Build NQ.'
	@echo '  chk      Run source code checks'
	@echo
	@echo 'Development Setup Targets:'
	@echo '  dep      Install linters.'
	@echo '  katex    Install KaTeX.'
	@echo '  rekatex  Reinstall KaTeX.'
	@echo
	@echo 'Development Utilities:'
	@echo '  lstags   List tags for each quiz.'
	@echo
	@echo 'Publication Targets:'
	@echo '  cc       Publish Git repository.'
	@echo '  cp       Copy NQ to susam.net.'


# Development Targets
# -------------------

all: nq chk

nq:
	sbcl --script nq.lisp

chk: tidy lint test

tidy:
	tidy -q -e --warn-proprietary-attributes no $(NAME).html

lint:
	npx standard --plugin html *.html

test:
	sbcl --noinform --eval "(defvar *quit* t)" --script test.lisp


# Development Setup Targets
# -------------------------

dep:
	npm install --no-save standard eslint-plugin-html
	if command -v brew; then brew install tidy-html5; fi

rekatex: rmkatex katex

rmkatex:
	rm -rf js/katex/

katex:
	! [ -e js/katex/ ]
	mkdir -p js/
	rm -f /tmp/katex.tar.gz
	curl -sSLo /tmp/katex.tar.gz https://github.com/KaTeX/KaTeX/releases/download/v0.19.1/katex.tar.gz
	tar -xvf /tmp/katex.tar.gz -C js/

# Development Utilities
# ---------------------

lstags:
	awk 'FNR == 3 { print FILENAME " " $$0 }' q/*.txt


# Publish Targets
# ---------------

cc:
	git remote remove cc || :
	git remote add cc git@codeberg.org:susam/$(NAME).git
	git remote set-url cc --add git@github.com:susam/$(NAME).git
	git remote remove origin || :
	git remote add origin "$$(git remote get-url cc)"
	git remote -v
	git push origin main --tags
	git push cc main --tags

cp:
	cp $(NAME).html ../susam.net/content/tree/
