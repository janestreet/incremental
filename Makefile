# Generic Makefile for oasis project

SETUP := setup.exe
NAME := incremental
PREFIX ?= $(shell grep ^prefix= setup.data | cut -d\" -f 2)

# Default rule
default: build

setup.exe: _oasis setup.ml
	ocamlfind ocamlopt -o $@ -linkpkg -package ocamlbuild,oasis.dynrun setup.ml || \
	  ocamlfind ocamlc -o $@ -linkpkg -package ocamlbuild,oasis.dynrun setup.ml || true
	for f in setup.*; do [ $$f = $@ -o $$f = setup.ml ] || rm -f $$f; done

build: $(SETUP) setup.data
	./$(SETUP) -build $(BUILDFLAGS)
	$(MAKE) $(NAME).install

doc: $(SETUP) setup.data build
	./$(SETUP) -doc $(DOCFLAGS)

test: $(SETUP) setup.data build
	./$(SETUP) -test $(TESTFLAGS)

all: $(SETUP)
	./$(SETUP) -all $(ALLFLAGS)
	$(MAKE) $(NAME).install

$(NAME).install: install.ml setup.log setup.data
	ocaml -I "$(OCAML_TOPLEVEL_PATH)" install.ml

install: $(NAME).install
	opam-installer -i --prefix $(PREFIX) $(NAME).install

uninstall: $(NAME).install
	opam-installer -u --prefix $(PREFIX) $(NAME).install

reinstall: $(NAME).install
	opam-installer -u --prefix $(PREFIX) $(NAME).install &> /dev/null || true
	opam-installer -i --prefix $(PREFIX) $(NAME).install

bin.tar.gz: $(NAME).install
	rm -rf _install
	mkdir _install
	opam-installer -i --prefix _install $(NAME).install
	tar czf bin.tar.gz -C _install .
	rm -rf _install

bin.lzo: $(NAME).install
	rm -rf _install
	mkdir _install
	opam-installer -i --prefix _install $(NAME).install
	cd _install && lzop -1 -P -o ../bin.lzo `find . -type f`
	rm -rf _install

clean: $(SETUP)
	./$(SETUP) -clean $(CLEANFLAGS)

distclean: $(SETUP)
	./$(SETUP) -distclean $(DISTCLEANFLAGS)

configure: $(SETUP)
	./$(SETUP) -configure $(CONFIGUREFLAGS)

setup.data: $(SETUP)
	./$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: default build doc test all install uninstall reinstall clean distclean configure
