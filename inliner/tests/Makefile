all: set

set: setint.opt setmain.cmx

ASMOPT=../asmopt.opt
ASMFLAGS= -do_combine -ext opt -c
OCAMLC=ocamlc
OCAMLDEP=ocamldep
OCAMLOPT=ocamlopt -S 

depend: $(TMPFILES)
	$(OCAMLDEP) $(INCLUDES)  *.ml *.mli > .depend

clean:
	rm -f *.s *.cm[iox] *.o

.SUFFIXES: .mli .ml .cmx .cmo .o .c .cmi .out .exe .mll .mly .cfg .ps .gv .mlig .s .opt

.s.opt:
	$(ASMOPT) $(ASMFLAGS) $<

.ps.gv:
	gv $<

.cfg.ps:
	dot -Tps < $*.cfg > $*.ps

.cmx.cfg:
	./asmopt $*.s

.mli.cmi :
	$(OCAMLC) $(INCLUDES) -c $<

.cmx.s:
	echo done

.ml.cmi :
	$(OCAMLC) $(INCLUDES) -c $<

.ml.cmx :
	$(OCAMLOPT) $(INCLUDES) -c $<

.ml.cmo :
	$(OCAMLC) $(INCLUDES) -c $<

.mll.ml :
	ocamllex $<

.mly.ml :
	ocamlyacc $<

.mly.mli:
	ocamlyacc $<

.c.o :
	$(OCAMLC) -o $*.o -c $<

.ml.mlig:
	cp .header.ml $*.mlig
	$(OCAMLC) $(INCLUDES) -i -c $< >> $*.mlig
	mv $*.mlig $*.mli

include .depend
