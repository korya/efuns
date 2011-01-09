include ../Makefile.config

TARGETS=versionpatch

all: byte

byte: versionpatch
opt: versionpatch
install:
installopt:

versionpatch: versionpatch.ml
	$(OCAMLC) -o versionpatch versionpatch.ml

clean:
	rm -f *.cm? *.mlg *.o *~ *.output core *.s $(TMPFILES) $(TARGETS) *.cma *.cmxa *.a

depend: fdepend

include .depend
include ../Makefile.rules
