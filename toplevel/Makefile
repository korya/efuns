include ../Makefile.config

TMPFILES=dynlink.ml

LIB=toplevel

# We don't need all these files. Only linking is important. After, we can
# load all the files to compile.

INCLUDES=-I ../ocamlsrc/utils -I ../ocamlsrc/parsing -I ../ocamlsrc/typing -I ../ocamlsrc/bytecomp -I ../ocamlsrc/asmcomp -I ../ocamlsrc/driver -I ../ocamlsrc/toplevel -I ../common -I ../ocamlsrc

TOPLEVELMAIN=topmain.cmo

# The bytecode compiler compiled with the native-code compiler

all: byte opt

byte: $(LIB).cma dyneval.cmo

opt: $(LIB).cmxa lib$(LIB).a dyneval.cmx


COMPFLAGS= $(INCLUDES)
DEPFLAGS= $(INCLUDES)

DYNLINK=bytecode/dynsyms.cmo dynlink.cmo
DYNLINKOPT= interp.cmx asmcode/dynsyms.cmx dynlink.cmx

cmorun:
	ocamlopt -o cmorun libtoplevel.a ../ocamlsrc/toplevellib.cmxa ../common/common.cmxa  toplevel.cmxa  -I ../ocamlsrc/utils -I ../ocamlsrc/parsing -I ../ocamlsrc/typing -I ../ocamlsrc/bytecomp -I ../ocamlsrc/asmcomp -I ../ocamlsrc/driver -I ../ocamlsrc/toplevel -I ../common cmorun.ml

COBJS= bytecompat.o getglobal.o

TOPLIB=$(DYNLINK)  $(TOPLEVEL)
OPTTOPLIB= $(DYNLINKOPT) $(TOPLEVEL:.cmo=.cmx) 

$(LIB).cma: $(TOPLIB)
	$(OCAMLC) -a -o $(LIB).cma $(TOPLIB)

$(LIB).cmxa:  $(OPTTOPLIB)
	$(OCAMLOPT) -a -o $(LIB).cmxa $(OPTTOPLIB)

lib$(LIB).a: $(COBJS)
	ar rc lib$(LIB).a $(COBJS)
	$(RANLIB) lib$(LIB).a

bytecode/dynsyms.mli: dynsyms.mli
	rm -f bytecode/dynsyms.mli
	ln -s ../dynsyms.mli bytecode/dynsyms.mli
bytecode/dynsyms.cmi: dynsyms.cmi
	rm -f bytecode/dynsyms.cmi
	ln -s ../dynsyms.cmi bytecode/dynsyms.cmi

bytecode/dynsyms.cmo: bytecode/dynsyms.cmi
bytecode/dynsyms.cmi: bytecode/dynsyms.mli

asmcode/dynsyms.mli: dynsyms.mli
	rm -f asmcode/dynsyms.cmli
	ln -s ../dynsyms.mli asmcode/dynsyms.mli
asmcode/dynsyms.cmi: dynsyms.cmi
	rm -f asmcode/dynsyms.cmi
	ln -sf ../dynsyms.cmi asmcode/dynsyms.cmi

asmcode/dynsyms.cmx: asmcode/dynsyms.cmi asmcode/dynsyms.ml
asmcode/dynsyms.cmi: asmcode/dynsyms.mli
dynlink.cmx:  asmcode/dynsyms.cmx


clean:
	rm -f *.cmxa
	rm -f *.cm[oiax] *~ ocaml *.opt *.o *.a
	rm -f  asmcode/*.cm[iox] asmcode/*.o
	rm -f  bytecode/*.cm[iox] bytecode/*.o
	rm -f asmcode/dynsyms.mli bytecode/dynsyms.mli
	rm -f $(TMPFILES)

depend: $(TMPFILES)
	$(OCAMLDEP) $(DEPFLAGS) *.mli *.ml bytecode/*.ml asmcode/*.ml > .depend

install:
	cp *.cmi $(LIB).cma dyneval.cmo $(installroot)$(INSTALLDIR)

installopt:
	cp *.cm[ix] $(LIB).cma $(LIB).cmxa lib$(LIB).a $(LIB).a dyneval.cmx dyneval.o $(installroot)$(INSTALLDIR)

dynlink.cmx: dynlink.cmi
dynlink.cmo: dynlink.cmi
dynlink.cmi: dynlink.mli

interp.cmx: interp.ml
	$(OCAMLOPT) $(INCLUDES) -c -unsafe interp.ml

include .depend
include ../Makefile.rules


