include ../../Makefile.config

INCLUDES=-I ..

BYTE_EXES=pacman.out xprop.out xpmroot.out chess.out xpm2ml.out sqmove.out movePointer.out sendKey.out xbanner.out
OPT_EXES=pacman.exe xprop.exe xpmroot.exe chess.exe xpm2ml.exe sqmove.exe movePointer.exe sendKey.exe xbanner.exe

all: byte
byte:$(BYTE_EXES)
opt: $(OPT_EXES)

clean: fclean
	rm -f *.out *.exe

depend: 
	ocamldep $(INCLUDES) *.ml > .depend

.SUFFIXES: .ml .out .exe
.ml.out:
	$(OCAMLCL) $(LIBDIR) $(BYTE_ALL_X) -o $*.out $*.ml

.ml.exe:
	$(OCAMLOPT) $(LIBDIR) $(OPT_ALL_X) -o $*.exe $*.ml

include .depend
include ../../Makefile.rules
