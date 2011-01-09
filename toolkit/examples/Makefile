include ../../Makefile.config

all: byte opt

INCLUDES=-I $(CURDIR)/toolkit -I $(CURDIR)/xlib -I $(CURDIR)/common -I $(CURDIR)/concur

DEMOS= demo_test.out demo_cursors.out demo_text.out demo_file.out demo_appli.out demo_panel.out demo_filesel.out demo_graphics.out demo_calc.out demo_table.out demo_tree.out demo_notebook.out demo_tex.out demo_bar.out demo_viewport.out demo_hello.out demo_hello2.out demo_complex.out demo_treesel.out demo_button.out

OPTDEMOS=$(DEMOS:.out=.exe)

$(DEMOS): $(CURDIR)/toolkit/WXlib.cma

byte: $(DEMOS)

opt: $(OPTDEMOS)

demo_filesel.out: demo_filesel.cmo
	$(OCAMLCL) -custom $(INCLUDES) -o $*.out $(BYTE_ALL_X) $(BYTE_LINK_STR) $(BYTE_LINK_COM) WXlib.cma $*.cmo

demo_filesel.exe: demo_filesel.cmx
	$(OCAMLOPT) $(INCLUDES) -o $*.exe $(OPT_ALL_X) $(OPT_LINK_STR) $(OPT_LINK_COM) WXlib.cmxa $*.cmx


demo_treesel.out: demo_treesel.cmo demo_mixer.o
	$(OCAMLCL) -custom $(INCLUDES) -o $*.out $(BYTE_ALL_X) $(BYTE_LINK_STR) $(BYTE_LINK_COM) WXlib.cma $*.cmo demo_mixer.o

demo_treesel.exe: demo_treesel.cmx demo_mixer.o
	$(OCAMLOPT) $(INCLUDES) -o $*.exe $(OPT_ALL_X) $(OPT_LINK_STR) $(OPT_LINK_COM) WXlib.cmxa $*.cmx demo_mixer.o

demo_tex.out: demo_tex.cmo
	$(OCAMLCL) -custom $(INCLUDES) -o $*.out $(BYTE_ALL_X) $(BYTE_LINK_STR) $(BYTE_LINK_COM) WXlib.cma $*.cmo

demo_tex.exe: demo_tex.cmx
	$(OCAMLOPT) $(INCLUDES) -o $*.exe $(OPT_ALL_X) $(OPT_LINK_STR) $(OPT_LINK_COM) WXlib.cmxa $*.cmx

clean: fclean
	rm -f *.out *.exe

depend: fdepend

.SUFFIXES: .out .exe .cmo .cmx

.cmx.exe:
	$(OCAMLOPT) $(INCLUDES) $(LIBDIR) $(OPT_LINK_X) -o $*.exe $(OPT_ALL_X) $(OPT_LINK_COM) WXlib.cmxa $*.cmx

.cmo.out: 
	$(OCAMLCL) -custom $(INCLUDES) -o $*.out $(BYTE_ALL_X) $(BYTE_LINK_COM) WXlib.cma $*.cmo

include .depend
include ../../Makefile.rules
