PACKAGENAME=markdown
COLLECTS=markdown

all: setup

clean:
	find . -name compiled -type d | xargs rm -rf
	rm -rf htmldocs

setup:
	raco setup $(COLLECTS)

link:
	raco pkg install --link -n $(PACKAGENAME) $$(pwd)

unlink:
	raco pkg remove $(PACKAGENAME)

test: setup
	raco test -x .

test-slow: setup
	raco test -x -s slow-test .

test-all: test test-slow

