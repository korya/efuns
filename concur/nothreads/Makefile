include ../../Makefile.config

INCLUDES= -I ../../common
FILES= concur.ml
CFILES=select.o

all: byte opt
byte: ../concur.cma ../libconcur.a
opt: ../concur.cmxa ../libconcurnat.a

../concur.cma ../libconcur.a: $(FILES:.ml=.cmo) $(CFILES)
	$(OCAMLC) -a -o ../concur.cma  $(FILES:.ml=.cmo)
	ar rc ../libconcur.a $(CFILES)

../concur.cmxa ../libconcurnat.a: $(FILES:.ml=.cmx) $(CFILES)
	$(OCAMLOPT) -a -o ../concur.cmxa $(FILES:.ml=.cmx)
	ar rc ../libconcurnat.a $(CFILES)
	cp concur.cmx ../concur.cmx

clean: 
	rm -f *.cm[iaxo] *.[ao] *.cmxa core *~ concur.mli

depend:
	$(OCAMLDEP) $(INCLUDES) *.ml* > .depend

concur.ml: concur.cmi
concur.mli: ../concur.mli
	rm -f concur.mli
	ln -s ../concur.mli concur.mli
concur.cmi: concur.mli ../concur.cmi
	rm -f concur.cmi
	ln -s ../concur.cmi concur.cmi

include .depend
include ../../Makefile.rules
