include ../Makefile.config

all: byte

tmpfiles: $(TMPFILES)

INTERFACES=concur.cmi

byte: $(INTERFACES)
	cd $(BYTE_THREADS); $(MAKE) byte

opt: $(INTERFACES)
	cd $(OPT_THREADS); $(MAKE) opt

install: 
	mkdir -p $(installroot)$(INSTALLDIR)
	mkdir -p $(installroot)$(INSTALLDIR)/sources
	cp $(INTERFACES)  $(installroot)$(INSTALLDIR)/
	cp *.mli  $(installroot)$(INSTALLDIR)/sources
	cp concur.cma libconcur.a $(installroot)$(INSTALLDIR)

installopt: 
	mkdir -p $(installroot)$(INSTALLDIR)
	cp $(INTERFACES)  $(installroot)$(INSTALLDIR)/
	cp concur.cmxa concur.a libconcurnat.a $(installroot)$(INSTALLDIR)

clean:
	rm -f *.cm[iaxo] *.[ao] *.cmxa core *~ $(TMPFILES) 
	(cd $(BYTE_THREADS); $(MAKE) clean)
	(cd $(OPT_THREADS); $(MAKE) clean)

depend: tmpfiles
	$(OCAMLDEP)  *.ml *.mli > .depend
	(cd $(BYTE_THREADS); $(MAKE) depend)
	(cd $(OPT_THREADS); $(MAKE) depend)

include .depend
include ../Makefile.rules
