include ../Makefile.config

FILES= xdebug.ml xA.ml xfifo.ml equeue.ml jeton.ml xtypes.ml wintbl.ml xbuffer.ml xauth.ml ll_trans.ml conv_event.ml display.ml xproto.ml x.ml xsync.ml xasync.ml xC.ml xK.ml keyBind.ml xlib.ml xtext.ml icccm.ml zpixmap.ml  shape.ml xmu.ml eloop.ml selection.ml xterm.ml xpm.ml xGraphics.ml xrm.ml


SS= $(FILES:.ml=.s)
OBJS= $(FILES:.ml=.cmo) 
INTERFACES=$(FILES:.ml=.cmi)
TMPFILES=xK.ml xrm.ml xpm.ml


INCLUDES= $(LIBDIR)

all: byte

demos.byte: xlib.cma
	(cd examples; $(MAKE) byte)

demos.opt: xlib.cmxa
	(cd examples; $(MAKE) opt)

demos: demos.byte demos.opt

# this is for JoCaml (http://pauillac.inria.fr/jocaml)
joxrun:
	jocamlc -join -make_vm -o joxrun -I ../concur unix.cma join.cma concur.cma xlib.cma

tmpfiles: $(TMPFILES)

mlis:
	$(MAKE) all
	$(MAKE) $(OBJS:.cmo=.mlii)
	mv mliis/*.mli .
	$(MAKE) clean
	$(MAKE) depend
	$(MAKE)

top: xlib.cma
	ocamlmktop -custom -o ocamlx $(BYTE_ALL_X)

byte: $(TMPFILES) xlib.cma xlib_config.byte

opt: $(TMPFILES) xlib.cmxa xlib_config.opt


xlib_config.byte: xlib_config.cmo
	$(OCAMLCL) -o xlib_config.byte $(BYTE_LINK_COM) xlib_config.cmo

xlib_config.opt: xlib_config.cmx
	$(OCAMLOPT) -o xlib_config.opt $(OPT_LINK_COM) xlib_config.cmx

xlib.cma: $(FILES:.ml=.cmo) Makefile
	$(OCAMLC) -a -o xlib.cma  $(FILES:.ml=.cmo)

ASMOPT=asmopt.opt -stats -ext Sopt -c

optopt:
	$(ASMOPT) $(SS)
	$(OCAMLOPT) -a -o xlib.cmxa $(FILES:.ml=.cmx)

xlib.cmxa: $(FILES:.ml=.cmx)
	$(OCAMLOPT) -a -o xlib.cmxa $(FILES:.ml=.cmx)

xK.ml: xK.ml.cpp Makefile
	cp xK.ml.cpp xK.ml
	echo >> xK.ml
	echo "let name_to_keysym = [" >> xK.ml
	cat xK.ml.cpp | sed -n -e 's/let.*xk_\([a-zA-Z_0-9]*\).*=.*0x\([0-9A-Fa-f]*\).*$$/"\1",0x\2;/p' >> xK.ml
	echo "]" >> xK.ml
	echo "let keysym_to_name = [" >> xK.ml
	cat xK.ml.cpp | sed -n -e 's/let.*xk_\([a-zA-Z_0-9]*\).*=.*0x\([0-9A-Fa-f]*\).*$$/0x\2,"\1";/p' >> xK.ml
	echo "]" >> xK.ml

install: 
	mkdir -p $(installroot)$(INSTALLDIR)
	mkdir -p $(installroot)$(INSTALLDIR)/sources
	cp *.mli  $(installroot)$(INSTALLDIR)/sources
	cp $(INTERFACES) xlib.cma  $(installroot)$(INSTALLDIR)/
	mkdir -p $(installroot)$(INSTALLBIN)
	cp -f xlib_config.byte $(installroot)$(INSTALLBIN)/xlib_config

installopt: 
	mkdir -p $(installroot)$(INSTALLDIR)
	cp $(INTERFACES) *.cmx xlib.a xlib.cmxa $(installroot)$(INSTALLDIR)/
	mkdir -p $(installroot)$(INSTALLBIN)
	cp -f xlib_config.opt $(installroot)$(INSTALLBIN)/xlib_config
	cp -f *.mli  $(installroot)$(INSTALLDIR)/sources
	(for i in $(OBJS:.cmo=.ml); do \
	if [ -e "$$i"l ]; then \
	  cp "$$i"l $(installroot)$(INSTALLDIR)/sources; \
	elif [ -e "$$i"y ]; then \
	  cp "$$i"y $(installroot)$(INSTALLDIR)/sources; \
	else \
	 cp "$$i" $(installroot)$(INSTALLDIR)/sources; fi; done)
	mkdir -p $(installroot)$(INSTALLDIR)/examples
	cp -dpR examples/*.ml examples/images examples/plateau1.txt  $(installroot)$(INSTALLDIR)/examples/

clean: fclean
	rm -f ocamlx xlib_config.opt xlib_config.byte
	(cd examples; $(MAKE) clean)

depend: tmpfiles fdepend
	(cd examples; $(MAKE) depend)


include .depend
include ../Makefile.rules

.SUFFIXES: .cmo .mlii .ml .cmx .Sopt

.cmo.mlii:
	cp $(HOME)/devel/header $*.mli
	$(OCAMLC) $(INCLUDES) -c -i $*.ml >> $*.mli
	mv $*.mli mliis/$*.mli

.cmx.Sopt:
	$(ASMOPT) -stats -c -ext Sopt $*.s