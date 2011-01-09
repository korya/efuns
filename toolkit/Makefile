all: byte opt

include ../Makefile.config

INCLUDES= $(LIBDIR)

OBJS= wX_types.cmo wX_adjust.cmo wX_display.cmo wX_screen.cmo wX_base.cmo wX_object.cmo wX_root.cmo wX_top.cmo wX_dummy.cmo wX_bar.cmo wX_table.cmo wX_scrollbar.cmo wX_label.cmo wX_pixmap.cmo wX_viewport.cmo wX_text.cmo wX_button.cmo wX_popup.cmo wX_selector.cmo wX_deleg.cmo wX_port.cmo wX_wmtop.cmo  wX_appli.cmo wX_dialog.cmo wX_dummy.cmo wX_panel.cmo wX_scale.cmo wX_ledit.cmo wX_radiobutton.cmo wX_Graphics.cmo wX_xterm.cmo wX_filesel.cmo wX_tree.cmo wX_swap.cmo wX_notebook.cmo wX_parent.cmo

# keep: wX_filesel.mli wX_appli.mli wX_pixmap.mli
MLIS= wX_types.mli wX_adjust.mli wX_display.mli wX_screen.mli wX_base.mli wX_object.mli wX_root.mli wX_top.mli wX_dummy.mli wX_bar.mli wX_table.mli wX_scrollbar.mli wX_label.mli wX_pixmap.mli wX_viewport.mli wX_text.mli wX_button.mli wX_popup.mli wX_selector.mli wX_deleg.mli wX_port.mli wX_wmtop.mli  wX_appli.mli wX_dialog.mli wX_dummy.mli wX_panel.mli wX_scale.mli wX_ledit.mli wX_radiobutton.mli wX_Graphics.mli wX_xterm.mli wX_tree.mli wX_swap.mli wX_notebook.mli


TMPFILES=

WXLIB=$(INSTALLDIR)
######################### special rules

tmpfiles: $(TMPFILES)

# demos


$(DEMOS): WXlib.cma
$(OPTDEMOS): WXlib.cmxa

demos.byte: WXlib.cma
	(cd examples; $(MAKE) byte)

demos.opt: WXlib.cmxa
	(cd examples; $(MAKE) opt)

demos: demos.byte demos.opt

byte: WXlib.cma $(CONFIG) tmpfiles wX_config.byte

opt: WXlib.cmxa $(CONFIG) tmpfiles wX_config.opt


mlis: 
	rm -f $(MLIS)
	$(MAKE) clean
	$(MAKE) $(MLIS:.mli=.mlii)
	(cd ..; $(MAKE) depend)
	$(MAKE) clean
	$(MAKE) byte

top: byte
	ocamlmktop -custom -o top unix.cma str.cma ../common/common.cma ../concur/concur.cma ../xlib/xlib.cma WXlib.cma -cclib "../concur/libconcur.a -lunix -lstr"

WXlib.cma: $(OBJS) $(BYTE_DEP_X) $(BYTE_DEP_COM)
	$(OCAMLCL) $(INCLUDES) -a -o WXlib.cma $(OBJS)

WXlib.cmxa: $(OBJS:.cmo=.cmx) $(OPT_DEP_X) $(OPT_DEP_COM) $(OPT_DEP_CCR)
	$(OCAMLOPT) $(INCLUDES) -a -o WXlib.cmxa  $(OBJS:.cmo=.cmx)

wX_config.byte: wX_config.cmo
	$(OCAMLCL) -o wX_config.byte $(BYTE_LINK_COM) wX_config.cmo

wX_config.opt: wX_config.cmx
	$(OCAMLOPT) -o wX_config.opt $(OPT_LINK_COM) wX_config.cmx

install: byte
	mkdir -p $(installroot)$(INSTALLBIN)
	mkdir -p $(installroot)$(WXLIB)
	mkdir -p $(installroot)$(WXLIB)/sources
	cp -f WXlib.cma *.cmi $(CONFIG) $(installroot)$(WXLIB)
	cp -f  wX_config.byte $(installroot)$(INSTALLBIN)/wX_config

installopt: opt
	mkdir -p $(installroot)$(INSTALLBIN)
	mkdir -p $(installroot)$(WXLIB)
	cp -f WXlib.cmxa WXlib.a *.cm[ix] $(CONFIG) $(installroot)$(WXLIB)
	cp -f wX_config.opt $(installroot)$(INSTALLBIN)/wX_config
	cp -f *.mli  $(installroot)$(WXLIB)/sources
	(for i in $(OBJS:.cmo=.ml); do \
	if [ -e "$$i"l ]; then \
	  cp "$$i"l $(installroot)$(WXLIB)/sources; \
	elif [ -e "$$i"y ]; then \
	  cp "$$i"y $(installroot)$(WXLIB)/sources; \
	else \
	 cp "$$i" $(installroot)$(WXLIB)/sources; fi; done)
	mkdir -p $(installroot)$(WXLIB)/examples
	cp examples/*.ml  $(installroot)$(WXLIB)/examples/

clean: fclean
	rm -f *.cm? *.mlg *.o WXlib.cma WXlib.cmxa WXlib.o WXlib.a *~ *.output core *.s $(TMPFILES)  $(DEMOS) top log *.byte *.opt
	(cd examples; $(MAKE) clean)

realclean: clean
	echo > .depend
	rm -f *.mli
	$(MAKE) depend

depend: tmpfiles fdepend
	(cd examples; $(MAKE) depend)

.SUFFIXES: .cmo .mlii

.cmo.mlii: $*.cmo
	cp $(HOME)/.rc/header.ml $*.mlii
	$(OCAMLC) $(INCLUDES) -c -i $*.ml >> $*.mlii
	mv $*.mlii $*.mli

include .depend
include ../Makefile.rules
