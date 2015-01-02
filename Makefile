PKG_NAME = $(shell oasis query name)
DIR = $(PKG_NAME)-$(shell oasis query version)
PKG_TARBALL = $(DIR).tar.gz

DISTFILES = _oasis _opam setup.ml _tags \
  $(wildcard $(addprefix src/, *.ml *.mli *.atd))


ATDGEN = src/dropbox_j.ml src/dropbox_j.mli src/dropbox_t.ml src/dropbox_t.mli

.PHONY: configure all byte native doc install uninstall reinstall
all byte native setup.log: setup.data
	ocaml setup.ml -build

configure: setup.data
setup.data: setup.ml
	ocaml setup.ml -configure --enable-tests --enable-docs

setup.ml: _oasis $(ATDGEN)
	oasis setup -setup-update dynamic

$(ATDGEN): src/dropbox.atd
	atdgen -j $<
	atdgen -t $<

doc install uninstall reinstall: setup.log
	ocaml setup.ml -$@

.PHONY: dist tar
dist tar: setup.ml
	mkdir -p $(DIR)
	for f in $(DISTFILES); do \
	  cp -r --parents $$f $(DIR); \
	done
# Make a setup.ml independent of oasis:
	cd $(DIR) && oasis setup
	tar -zcvf $(PKG_TARBALL) $(DIR)
	$(RM) -r $(DIR)



.PHONY: clean distclean
clean:
	ocaml setup.ml -clean
	$(RM) $(ATDGEN) $(PKG_TARBALL)

distclean:: clean
	ocaml setup.ml -distclean
	$(RM) $(wildcard *.ba[0-9] *.bak *~ *.odocl)
