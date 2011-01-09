all: byte opt

include ../Makefile.config

OFLAGS= -w mv
INCLUDES= -I $(CURDIR)/$(GWML_DYNLINK) $(LIBDIR) -I $(CURDIR)/toolkit

BYTE_LINK_DYN= $(GWML_BYTE_DYN_LINK) $(GWML_DYNLINK).cma $(CURDIR)/$(GWML_DYNLINK)/dyneval.cmo
OPT_LINK_DYN= $(GWML_OPT_DYN_LINK)  $(CURDIR)/$(GWML_DYNLINK)/lib$(GWML_DYNLINK).a $(CURDIR)/$(GWML_DYNLINK)/$(GWML_DYNLINK).cmxa $(CURDIR)/$(GWML_DYNLINK)/dyneval.cmx
OPT_DEP_DYN=  $(CURDIR)/$(GWML_DYNLINK)/lib$(GWML_DYNLINK).a $(CURDIR)/$(GWML_DYNLINK)/$(GWML_DYNLINK).cmxa $(CURDIR)/$(GWML_DYNLINK)/dyneval.cmx
BYTE_DEP_DYN=  $(CURDIR)/$(GWML_DYNLINK)/dyneval.cmo


AUTOLOAD=configurator.cmo
AUTOLOAD_ML=configurator.ml
AUTOLOAD_CMI=configurator.cmi

CONFIG= cpp.cmo sendEvent.cmo image.cmo opttable.cmo stdconfig.cmo animate.cmo  stdvirtual.cmo user.cmo stdmenus.cmo stdicon.cmo  stdplacement.cmo themes.cmo stddeco.cmo appMgr.cmo  modules.cmo fvwm.cmo wmaker.cmo afterstep.cmo twm_t.cmo twm_l.cmo twm_p.cmo twm.cmo group.cmo gnome.cmo  undo.cmo dvroom.cmo stdgwmlrc.cmo gwmlrc.cmo 

CONFIG_ML= cpp.mll sendEvent.ml image.ml opttable.ml stdconfig.ml animate.ml  stdvirtual.ml user.ml appMgr.ml  stdmenus.ml stdicon.ml  stdplacement.ml themes.ml stddeco.ml modules.ml fvwm.mll wmaker.mll afterstep.mll twm_t.ml twm_l.mll twm_p.mly twm.ml group.ml gnome.ml  undo.ml dvroom.ml stdgwmlrc.ml gwmlrc.ml 

CONFIG_CMI=$(CONFIG:.cmo=.cmi)

COBJS=$(IMLIB_COBJS) $(ESD_COBJS)
CFLAGS=$(IMLIB_CFLAGS) $(ESD_CFLAGS)
CLINK=$(IMLIB_CLINK) $(ESD_CLINK)

WIDGETS= wob.cmo plug.cmo top.cmo client.cmo screen.cmo null.cmo label.cmo ledit.cmo pixmap.cmo bar.cmo text.cmo
OBJS= gwml_args.cmo locale.cmo wobenv.cmo imager.cmo sound.cmo gwml.cmo gradients.cmo $(WIDGETS)

MAIN=main.cmo

TMPFILES= twm_p.mli twm_p.ml twm_l.ml fvwm.ml wmaker.ml cpp.ml afterstep.ml

gwml_install: Makefile
	rm -f gwml_install
	echo "#!/bin/sh" > gwml_install
	echo CONFIG_DIR='$$HOME'/.gwml-$(GWML_VERSION) >> gwml_install
	echo echo Installing in '$$CONFIG_DIR' >> gwml_install
	echo mkdir -p '$$CONFIG_DIR' >> gwml_install
	echo cd $(GWMLLIB)/config >> gwml_install
	echo cp -f "*" '$$CONFIG_DIR'/ >> gwml_install
	echo touch '$$CONFIG_DIR'/.depend >> gwml_install
	echo cd '$$CONFIG_DIR' >> gwml_install
	echo make depend >> gwml_install
	echo echo Installation done >> gwml_install
	echo echo Don''t forget to compile the files with make >> gwml_install
	echo echo after modification >> gwml_install
	chmod +x gwml_install
	rm -f config/Makefile
	echo > config/Makefile
	echo CONFIG=$(CONFIG) $(AUTOLOAD) >> config/Makefile
	echo >> config/Makefile
	echo TMPFILES=$(TMPFILES) >> config/Makefile
	echo >> config/Makefile
	echo all: "\$$""(TMPFILES)" "\$$""(CONFIG)" >> config/Makefile
	echo >> config/Makefile
	echo OCAMLC=$(INSTALLDIR)/$(OCAMLRUN) $(INSTALLDIR)/ocamlc >> config/Makefile
	echo OCAMLDEP=ocamldep >> config/Makefile
	echo INCLUDES=-I $(GWMLLIB) -I $(INSTALLDIR)  >> config/Makefile
	echo >> config/Makefile
	echo include Makefile.rules >> config/Makefile
	echo include .depend >> config/Makefile

######################### special rules

tmpfiles: $(TMPFILES)

byte: gwml.byte $(CONFIG) $(AUTOLOAD) tmpfiles gwml_install
	cp -f gwml.byte gwml

opt: gwml.opt $(CONFIG) $(AUTOLOAD) tmpfiles gwml_install
	cp -f gwml.opt gwml

top: 
	ocamlmktop -custom -o top unix.cma str.cma -cclib "-lunix -lstr"

gwml.byte: $(OBJS) $(BYTE_DEP_X) $(BYTE_DEP_COM) $(BYTE_DEP_DYN) $(COBJS) $(MAIN)
	$(OCAMLCL) $(INCLUDES) -o gwml.byte unix.cma $(BYTE_LINK_COM) $(BYTE_ALL_X) $(BYTE_LINK_DYN) $(OBJS) $(MAIN) $(COBJS) $(CLINK)

gwml.opt: $(OBJS:.cmo=.cmx) $(OPT_DEP_X) $(OPT_DEP_COM) $(OPT_DEP_DYN) $(OPT_DEP_CCR) $(COBJS) $(MAIN:.cmo=.cmx)
	$(OCAMLOPT) $(INCLUDES) -o gwml.opt unix.cmxa $(OPT_LINK_COM)  $(OPT_ALL_X) $(OPT_LINK_DYN) $(OBJS:.cmo=.cmx) $(MAIN:.cmo=.cmx) $(COBJS) $(CLINK)

gwml.static: $(OBJS:.cmo=.cmx) $(OPT_DEP_X) $(OPT_DEP_COM) $(OPT_DEP_DYN) $(OPT_DEP_CCR) $(COBJS) $(CONFIG:.cmo=.cmx) $(MAIN:.cmo=.cmx)
	$(OCAMLOPT) $(INCLUDES) -o gwml.static  $(OPT_ALL_X) $(OPT_LINK_COM) $(OPT_LINK_DYN) $(OBJS:.cmo=.cmx) $(CONFIG:.cmo=.cmx) $(MAIN:.cmo=.cmx) $(COBJS) $(CLINK)

install-static: gwml.static
	cp -f gwml.static $(installroot)$(INSTALLBIN)/gwml.static

install-icons:

THEMESDIR=$(CURDIR)/gwml/themes

install-themes:
	mkdir -p $(installroot)/usr/local/share/WindowMaker/
	(cd $(installroot)/usr/local/share/WindowMaker; \
	tar xf $(THEMESDIR)/WindowMaker.tar)
	mkdir -p  $(installroot)/usr/local/share/afterstep/desktop/themes
	(cd $(installroot)/usr/local/share/afterstep/desktop/themes;\
	tar xf $(THEMESDIR)/AfterStep.tar)
	mkdir -p $(installroot)/usr/local/share/GwML
	(cd $(installroot)/usr/local/share/GwML; tar xf $(THEMESDIR)/GwML.tar)

themes-tar:
	rm -rf themes.copy
	cp -dpR themes themes.copy
	(cd themes.copy; find . -name CVS -exec rm -rf {} \; ; echo)
	(cd themes.copy/WindowMaker; tar cf ../../themes/WindowMaker.tar .)
	(cd themes.copy/AfterStep; tar cf ../../themes/AfterStep.tar .)
	(cd themes.copy/GwML; tar cf ../../themes/GwML.tar .)
	rm -rf themes.copy

install-config: $(CONFIG) $(AUTOLOAD) gwml_install
	mkdir -p  $(installroot)$(GWMLLIB)/config
	cp -f $(CONFIG_ML) $(AUTOLOAD_ML)  $(installroot)$(GWMLLIB)/config
	cp -f config/Makefile config/Makefile.rules config/README $(installroot)$(GWMLLIB)/config
	cp -f gwml_install $(installroot)$(INSTALLBIN)/
	cp -f FAQ.gwml $(installroot)$(GWMLLIB)

install: install-config
	mkdir -p $(installroot)$(INSTALLBIN)
	cp -f gwml.byte $(installroot)$(INSTALLBIN)/gwml.byte
	mkdir -p $(installroot)$(GWMLLIB)
	cp -f *.cmi $(CONFIG) $(AUTOLOAD) $(installroot)$(GWMLLIB)

installopt: install-config
	mkdir -p $(installroot)$(INSTALLBIN)
	cp -f gwml.opt $(installroot)$(INSTALLBIN)/gwml
	mkdir -p $(installroot)$(GWMLLIB)
	cp -f *.cmi $(CONFIG) $(AUTOLOAD) $(installroot)$(GWMLLIB)

clean:
	rm -f *.cm? *.mlg *.o gwml gwml.byte gwml.opt *~ *.output core *.s $(TMPFILES) gwml.gz gwml.static top log 

depend: tmpfiles fdepend

tmp_p.mli: tmp_p.ml


include .depend
include ../Makefile.rules
