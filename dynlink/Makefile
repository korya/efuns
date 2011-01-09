include ../Makefile.config

TMPFILES=dynlink.ml

CSRCS=getglobal.c bytecompat.c
SRCS= interp.ml linker.ml dynlink.ml
LIB=dynlink


COBJS=$(CSRCS:.c=.o)
OBJS=$(SRCS:.ml=.cmx)
INTERFACES= linker.cmi dynlink.cmi
DYNOBJS= dyneval.cmo


INCLUDES= -I $(CURDIR)/common -I ../ocamlsrc/version/include

all: byte opt

byte: dyneval.cmo

opt: dynlink.cmxa  libdynlink.a dyneval.cmx


tmpfiles: $(TMPFILES)

example: ocamlcmorun example.cmo

lib$(LIB).a: $(COBJS) Makefile
	rm -f lib$(LIB).a
	ar rc lib$(LIB).a $(COBJS)
	$(RANLIB) lib$(LIB).a

$(LIB).cmxa: $(OBJS) $(COBJS) Makefile
	$(OCAMLOPT) -a -o $(LIB).cmxa $(OBJS)

ocamlcmorun: test.cmx lib$(LIB).a $(LIB).cmxa
	$(OCAMLOPT) -linkall -o ocamlcmorun -cclib lib$(LIB).a $(LIB).cmxa test.cmx


dyneval.mli: $(CURDIR)/toplevel/dyneval.mli
	rm -f dyneval.mli
	ln -s ../toplevel/dyneval.mli dyneval.mli

dyneval.cmi: dyneval.mli
dyneval.cmo: dyneval.cmi
dyneval.cmx: dyneval.cmi


install:
	mkdir -p $(installroot)$(INSTALLDIR)
	cp -f *.cmi $(installroot)$(INSTALLDIR)

installopt:
	mkdir -p $(installroot)$(INSTALLDIR)
	cp -f *.cmi $(LIB).cmxa $(LIB).a lib$(LIB).a $(installroot)$(INSTALLDIR)

clean: 
	rm -f *.cm? *~ *.[ao] *.cmxa ocamlcmorun *.glb *.mlg core dyneval.mli $(TMPFILES)

depend: $(TMPFILES) fdepend

interp.cmx: interp.ml
	$(OCAMLOPT) $(INCLUDES) -c -unsafe $<

include .depend
include ../Makefile.rules
