include ../Makefile.config
include Makefile.ocaml

topall: top.byte top.opt
top.byte: toplevellib.cma
top.opt: toplevellib.cmxa
ocamlopt:
	ln -s $(OCAMLSRC)/ocamlopt ocamlopt
ocamlc:
	ln -s $(OCAMLSRC)/ocamlc ocamlc

TOPLIB_EFUNS= $(TOPLIB) compat_comp.cmo

compat_comp.cmo: compat_comp.cmi

toplevellib.cma: $(TOPLIB_EFUNS)
	$(OCAMLC) -a -o toplevellib.cma $(TOPLIB_EFUNS)
toplevellib.cmxa: $(TOPLIB_EFUNS:.cmo=.cmx)
	$(OCAMLOPT) -a -o toplevellib.cmxa $(TOPLIB_EFUNS:.cmo=.cmx)

topclean:
	rm -f *.cm[ioax] *.cmxa *.[oa]

installopt:
	cp toplevellib.cmxa toplevellib.a version/include/*.cmi $(installroot)/$(INSTALLDIR)

installbyte:
	cp toplevellib.cma version/include/*.cmi $(installroot)/$(INSTALLDIR)