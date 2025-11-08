APP = nq.html

checks: tidy lint

tidy:
	tidy -q -e --warn-proprietary-attributes no $(APP)

lint:
	npx standard --plugin html *.html

deps:
	npm install --no-save standard eslint-plugin-html
	if command -v brew; then brew install tidy-html5; fi

cp:
	cp $(APP) ~/git/susam.net/content/tree

pub: cp
	cd ~/git/susam.net/ && make copub

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

nq:
	sbcl --script nq.lisp

test:
	sbcl --noinform --eval "(defvar *quit* t)" --script test.lisp
