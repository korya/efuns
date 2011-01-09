include ../../../../Makefile.config

NEEDED=emitcode.cmi printlambda.cmi

all: $(NEEDED)

clean:
	rm -f *.cmi

depend:
	ocamldep *.mli > .depend

include .depend
include ../../../../Makefile.rules