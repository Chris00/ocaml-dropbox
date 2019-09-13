PKGVERSION = $(shell git describe --always)

build:
	dune build @install @tests

test:
	dune runtest --force

install uninstall:
	dune $@

doc:
	dune build @doc
	sed -e 's/%%VERSION%%/$(PKGVERSION)/' --in-place \
	  _build/default/_doc/_html/dropbox/Dropbox/index.html

clean:
	dune clean

.PHONY: build test install uninstall doc clean
