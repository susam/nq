nq:
	sbcl --script nq.lisp

test:
	sbcl --noinform --eval "(defvar *quit* t)" --script test.lisp

checks: tidy lint

tidy:
	tidy -q -e --warn-proprietary-attributes no nq.html

lint:
	npx standard --plugin html *.html

deps:
	npm install --no-save standard eslint-plugin-html
	if command -v brew; then brew install tidy-html5; fi

katex:
	mkdir -p js/
	if ! [ -e js/katex/ ]; then \
	    echo Downloading KaTeX ...; \
	    curl -sSLo katex.tgz https://github.com/KaTeX/KaTeX/releases/download/v0.16.22/katex.tar.gz; \
	    tar -xvf katex.tgz -C js/; \
	else \
	    echo KaTeX is already present.; \
	fi
	rm -f katex.tgz

cu:
	cp -v nq.html ~/git/susam.net/content/tree/
	@echo "Creating subshell to review change and publish"
	cd ~/git/susam.net/ && git checkout cu && git status && git add -p && git commit --amend --reset-author && make pub
