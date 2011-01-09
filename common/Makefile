include ../Makefile.config

all: byte


OFLAGS=-I ../ocamlsrc/version/include
LIBDIR=
OBJS=str2.cmo time.cmo lexers.cmo parsers.cmo utils.cmo version.cmo log.cmo parse_file.cmo compat_run.cmo instructs.cmo options.cmo

TMPFILES=version.ml parsers.ml lexers.ml

######################### standard rules
lexers.ml: lexers.mll

version.ml: ../Makefile.config version.mlv
	cat version.mlv > version.ml
	echo let ocamllib = \"$(OCAMLLIB)\" >> version.ml
	echo >> version.ml
	echo let installdir = \"$(INSTALLDIR)\" >> version.ml
	echo >> version.ml
	echo let builder = \"$$USER\" >> version.ml
	echo >> version.ml
	echo let date = \"`date`\" >> version.ml
	echo >> version.ml
	echo let efuns_version = \"$(EFUNS_VERSION)\" >> version.ml
	echo >> version.ml
	echo let efuns_lib = \"$(EFUNSLIB)\" >> version.ml
	echo >> version.ml
	echo let gwml_version = \"$(GWML_VERSION)\" >> version.ml
	echo >> version.ml
	echo let gwml_lib = \"$(GWMLLIB)\" >> version.ml
	echo >> version.ml
	echo let src_dir = \"$(CURDIR)\" >> version.ml
	echo >> version.ml

tmpfiles: $(TMPFILES)

byte: common.cma tmpfiles

opt: common.cmxa tmpfiles

top: 
	ocamlmktop -custom -o top unix.cma str.cma -cclib "-lunix -lstr"

common.cma: $(OBJS)
	$(OCAMLC) -a -o common.cma $(OBJS)

common.cmxa: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) -a -o common.cmxa $(OBJS:.cmo=.cmx)

install: 
	mkdir -p $(installroot)$(INSTALLDIR)
	mkdir -p $(installroot)$(INSTALLDIR)/sources
	cp *.cmi common.cma $(installroot)$(INSTALLDIR)
	cp *.mli $(installroot)$(INSTALLDIR)/sources
	cp $(OCAMLLIB)/*.cmi $(installroot)$(INSTALLDIR)
	cp $(OCAMLLIB)/*.cma $(installroot)$(INSTALLDIR)
	cp $(OCAMLRUN) $(installroot)$(INSTALLDIR)/ocamlrun
	cp $(OCAMLC) $(installroot)$(INSTALLDIR)/ocamlc

installopt: 
	mkdir -p $(installroot)$(INSTALLDIR)
	cp *.cm[ix] common.cmxa common.a $(installroot)$(INSTALLDIR)
	cp -f *.mli  $(installroot)$(INSTALLDIR)/sources
	(for i in $(OBJS:.cmo=.ml); do \
	if [ -e "$$i"l ]; then \
	  cp "$$i"l $(installroot)$(INSTALLDIR)/sources; \
	elif [ -e "$$i"y ]; then \
	  cp "$$i"y $(installroot)$(INSTALLDIR)/sources; \
	else \
	 cp "$$i" $(installroot)$(INSTALLDIR)/sources; fi; done)

clean:
	rm -f *.cm? *.mlg *.o *~ *.output core *.s $(TMPFILES)  top *.cma *.cmxa *.a

depend: fdepend

include .depend
include ../Makefile.rules
