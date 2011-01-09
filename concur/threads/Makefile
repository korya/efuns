include ../../Makefile.config

FILES= concur.ml
CFILES=

all: byte opt
byte: ../concur.cma
opt: ../concur.cmxa

../concur.cma: $(FILES:.ml=.cmo) $(CFILES)
	$(OCAMLC) -a -o ../concur.cma $(THREADS_CMA)  $(FILES:.ml=.cmo)
	cp $(OCAMLLIB)/libthreads.a ../libconcur.a
	ar r ../libconcur.a $(CFILES)

../concur.cmxa: $(FILES:.ml=.cmx) $(CFILES)
	$(OCAMLOPT) -thread -a -o ../concur.cmxa threads.cmxa $(FILES:.ml=.cmx)
	cp $(OCAMLLIB)/libthreadsnat.a ../libconcurnat.a
	ar r ../libconcurnat.a $(CFILES)
	cp concur.cmx ../concur.cmx

clean: 
	rm -f *.cm[iaxo] *.[ao] *.cmxa core *~ concur.mli

depend:
	$(OCAMLDEP)  *.ml* > .depend

concur.ml: concur.cmi
concur.mli:
	rm -f concur.mli
	ln -s ../concur.mli concur.mli
concur.cmi: concur.mli
	rm -f concur.cmi
	ln -s ../concur.cmi concur.cmi

.SUFFIXES:  .mli .ml .cmx .cmo .o .c .cmi .exe .run

.mli.cmi :
	$(OCAMLC) -thread -c $<

.ml.cmi :
	$(OCAMLC) -thread -c $<

.ml.cmx :
	$(OCAMLOPT) -thread -c $<

.ml.cmo :
	$(OCAMLC) -thread -c $<

.c.o :
	$(OCAMLC) -c $<


include .depend
