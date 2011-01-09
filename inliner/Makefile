ARCH=i386

INCLUDES= -I $(ARCH)/ -I combiner/

LIB_SRCS=ocamldot.ml utils.ml dPrintf.ml
FRONT_SRCS=args.ml misc.ml 
ARCH_SRCS= $(ARCH)/asm.ml match.ml $(ARCH)/archlexer.ml $(ARCH)/archparser.ml $(ARCH)/emit.ml
COMBINER=combiner/comb_types.ml combiner/comb_opt.ml combiner/comb_alloc.ml combiner/comb_transl.ml combiner/comb_main.ml
OPT_SRCS=print_cfg.ml cfg.ml approx.ml liveness.ml macros.ml toposort.ml po.ml inline.ml optimize1.ml peephole.ml optimize4.ml optimize3.ml optimize2.ml $(COMBINER) analysis.ml info.ml
BACK_SRCS=main.ml

SRCS=$(LIB_SRCS) $(FRONT_SRCS) $(ARCH_SRCS) $(OPT_SRCS) $(BACK_SRCS)

CMOS=$(SRCS:.ml=.cmo)
CMXS=$(SRCS:.ml=.cmx)
MLIS=$(SRCS:.ml=.mlig)
OPTS=$(SRCS:.ml=.Sopt)
SS=$(SRCS:.ml=.s)
SSS=$(SRCS:.ml=.Snor)
DIFFS=$(SRCS:.ml=.diff)

OCAMLC=ocamlc.opt
OCAMLDEP=ocamldep
OCAMLOPT=ocamlopt.opt -S 
ASMOPT=./asmopt.opt -I i386 -stats
# -I $(HOME)/devel/optimizer/ocaml-2.02/stdlib

TESTS=test1.ps test2.ps

TMPFILES=$(ARCH)/archlexer.ml $(ARCH)/archparser.ml $(ARCH)/archparser.mli $(TESTS:.ps.cmx)
TARGETS=asmopt asmopt.opt

all: byte $(TMPFILES)
byte: asmopt
opt: asmopt.opt

install: byte opt
	cp asmopt asmopt.opt /usr/local/bin/ocaml-2.02+asmopt/

mlis: $(MLIS)
opts: $(OPTS)

asmopt.opt.opt: matcher_opt.o $(CMXS)
	for i in $(SS); do $(ASMOPT) $$i; done
	$(OCAMLOPT) -o asmopt.opt.opt matcher_opt.o $(CMXS)	

sss: $(SSS)

tests: byte $(TESTS)

asmopt: matcher_byte.o $(CMOS)
	$(OCAMLC) -custom -o $@ matcher_byte.o $(CMOS)

asmopt.opt: matcher_opt.o $(CMXS)
	$(OCAMLOPT) -o $@ matcher_opt.o $(CMXS)

matcher_byte.c:
	ln -s matcher.c matcher_byte.c

matcher_byte.o: matcher.c matcher_byte.c
	$(OCAMLC) -c matcher_byte.c

matcher_opt.c:
	ln -s matcher.c matcher_opt.c

matcher_opt.o: matcher_opt.c matcher.c
	$(OCAMLC) -c -ccopt "-DNATIVE_CODE" matcher_opt.c

diffs: $(DIFFS)

test: testclean
	$(MAKE) -k opts sss diffs

DIRS=. $(ARCH) tests

testclean:
	for i in $(DIRS); do (cd $$i; rm -f *.S* *.opt *.diff); done

$(ARCH)/archlexer.ml: $(ARCH)/archlexer.mll
$(ARCH)/archparser.ml: $(ARCH)/archparser.mly $(ARCH)/archparser.mli
$(ARCH)/archparser.mli: $(ARCH)/archparser.mly

CLEAN_REGEXPS= *.cm[iaxo] *.[ao] *.cmxa core *.mlg *.out *.exe *~ *.s *.mlig *.cfg *.ps *.opt *.S* *.diff *.asx *.idx

clean: 
	rm -f $(CLEAN_REGEXPS) $(TMPFILES) $(TARGETS)
	(cd $(ARCH);rm -f $(CLEAN_REGEXPS) $(TMPFILES) $(TARGETS))
	(cd tests;rm -f $(CLEAN_REGEXPS) $(TMPFILES) $(TARGETS))

depend: $(TMPFILES)
	$(OCAMLDEP) $(INCLUDES) $(SRCS) $(ARCH)/*.ml $(ARCH)/*.mli > .depend


$(TESTS): asmopt

.SUFFIXES: .mli .ml .cmx .cmo .o .c .cmi .out .exe .mll .mly .cfg .ps .gv .mlig .s .Sopt .Snor .diff

.ps.gv:
	gv $<

.cfg.ps:
	dot -Tps < $*.cfg > $*.ps

.cmx.cfg:
	$(ASMOPT) $*.s

.mli.cmi :
	$(OCAMLC) $(INCLUDES) -c $<

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

.cmx.Sopt:
	$(ASMOPT) -c -ext Sopt $*.s

.cmx.Snor:
	grep -v "$asmopt" $*.s | grep -v "\.data" | grep -v "\.text" > $*.Snor

.cmx.diff:
	diff $*.Sopt $*.Snor > $*.diff

include .depend
