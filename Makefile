include Makefile.config


DISTRIB=$(HOME)/public_html/src/efuns
FTP_DIR=/net/pauillac/infosystems/www/para/cdrom/ftp/efuns
EFUNS_TAR=efuns-$(VERSION).src.tar.gz

ALLLIBS=tools.opt common.opt concur.opt xlib.opt wXlib.opt dynlink.opt $(DYNLINK).opt \
   $(GWML_DYNLINK).opt gwml.opt $(EFUNS_DYNLINK).opt efuns.opt
GWMLLIBS=common.opt concur.opt xlib.opt $(GWML_DYNLINK).opt gwml.opt
EFUNSLIBS=common.opt concur.opt xlib.opt wXlib.opt $(EFUNS_DYNLINK).opt efuns.opt

all: byte opt
byte: $(ALLLIBS:.opt=.byte)
opt: $(ALLLIBS)
install: installbyte installopt
installbyte: $(ALLLIBS:.opt=.installbyte)
installopt: $(ALLLIBS:.opt=.installopt)

gwml: $(GWMLLIBS)
efuns: $(EFUNSLIBS)
install-gwml: $(GWMLLIBS:.opt=.installopt)
	$(MAKE) install-icons install-themes
install-efuns: $(EFUNSLIBS:.opt=.installopt)


tools.opt:
	(cd tools; $(MAKE) opt)
tools.byte:
	(cd tools; $(MAKE) byte)
tools.installbyte:
	(cd tools; $(MAKE) install)
tools.installopt:
	(cd tools; $(MAKE) installopt)

common.opt:
	(cd ocamlsrc/version/include; $(MAKE))
	(cd common; $(MAKE) opt)
common.byte:
	(cd ocamlsrc/version/include; $(MAKE))
	(cd common; $(MAKE) byte)
common.installbyte:
	(cd common; $(MAKE) install)
common.installopt:
	(cd common; $(MAKE) installopt)




concur.opt:
	(cd concur; $(MAKE) opt)
concur.byte:
	(cd concur; $(MAKE) byte)
concur.installbyte:
	(cd concur; $(MAKE) install)
concur.installopt:
	(cd concur; $(MAKE) installopt)




xlib.opt:
	(cd xlib; $(MAKE) opt)
xlib.byte:
	(cd xlib; $(MAKE) byte)
xlib.installbyte:
	(cd xlib; $(MAKE) install)
xlib.installopt:
	(cd xlib; $(MAKE) installopt)




wXlib.opt:
	(cd toolkit; $(MAKE) opt)
wXlib.byte:
	(cd toolkit; $(MAKE) byte)
wXlib.installbyte:
	(cd toolkit; $(MAKE) install)
wXlib.installopt:
	(cd toolkit; $(MAKE) installopt)




dynlink.opt:
	(cd dynlink; $(MAKE) opt)
dynlink.byte:
	(cd dynlink; $(MAKE) byte)
dynlink.installopt:
	(cd dynlink; $(MAKE) installopt)
dynlink.installbyte:
	(cd dynlink; $(MAKE) install)




toplevel.opt:
	(cd ocamlsrc; $(MAKE) top.opt)
	(cd toplevel; $(MAKE) opt)
toplevel.byte:
	(cd ocamlsrc; $(MAKE) top.byte)
	(cd toplevel; $(MAKE) byte)
toplevel.installopt:
	(cd ocamlsrc; $(MAKE) installopt)
	(cd toplevel; $(MAKE) installopt)
toplevel.installbyte:
	(cd ocamlsrc; $(MAKE) installbyte)
	(cd toplevel; $(MAKE) install)



gwml.opt:
	(cd gwml; $(MAKE) opt)
gwml.byte:
	(cd gwml; $(MAKE) byte)
gwml.installopt:
	(cd gwml; $(MAKE) installopt install-icons install-themes)
gwml.installbyte:
	(cd gwml; $(MAKE) installopt install-icons install-themes)


efuns.opt: 
	(cd efuns; $(MAKE) opt)
efuns.byte: 
	(cd efuns; $(MAKE) byte)
efuns.installopt: 
	(cd efuns; $(MAKE) installopt)
efuns.installbyte: 
	(cd efuns; $(MAKE) install)


commit:
	cvs commit -m "Version $(VERSION)"

update:
	cvs update; $(MAKE) depend

demos.byte:
	(cd xlib; $(MAKE) demos.byte)
	(cd toolkit; $(MAKE) demos.byte)
demos.opt:
	(cd xlib; $(MAKE) demos.opt)
	(cd toolkit; $(MAKE) demos.opt)

demos: demos.byte demos.opt

tar:
	rm -rf $(HOME)/distrib/efuns
	rm -rf $(HOME)/distrib/efuns-$(VERSION)
	(cd $(HOME)/distrib; cvs co efuns; mv efuns efuns-$(VERSION); tar zcf $(EFUNS_TAR) efuns)
	rm -f $(FTP_DIR)/$(EFUNS_TAR)
	rm -f $(FTP_DIR)/efuns.src.tar.gz
	cp $(HOME)/distrib/efuns-$(VERSION).tar.gz $(FTP_DIR)/$(EFUNS_TAR)
	ln -s $(EFUNS_TAR) $(FTP_DIR)/efuns.src.tar.gz

distrib: commit
	mkdir -p $(HOME)/distrib/test
	(cd $(HOME)/distrib/test;rm -rf efuns;cvs co efuns;cd efuns;$(MAKE) byte opt)
	(cd $(HOME)/distrib;rm -rf efuns;cvs co efuns)
	(cd $(HOME)/distrib;tar zcf $(DISTRIB)/efuns-$(VERSION).src.tar.gz efuns)
	ln -sf efuns-$(VERSION).src.tar.gz  $(DISTRIB)/efuns.tar.gz

specialix: commit
	(cd $(HOME)/specialix/devel/efuns;cvs update)

depend:
	(cd tools; make versionpatch)
	(cd ocamlsrc/version/include; $(MAKE) depend) 
	(cd concur; $(MAKE) depend)
	(cd xlib; $(MAKE) depend)
	(cd common; $(MAKE) depend)
	(cd toolkit; $(MAKE) depend)
	(cd dynlink; $(MAKE) depend)
	(cd $(DYNLINK); $(MAKE) depend)
	(cd efuns; $(MAKE) depend)
	(cd gwml; $(MAKE) depend)

clean: 
	rm -f *~ core version.ml
	(cd tools; $(MAKE) clean)
	(cd ocamlsrc; $(MAKE) topclean)
	(cd concur; $(MAKE) clean)
	(cd xlib; $(MAKE) clean)
	(cd dynlink; $(MAKE) clean)
	(cd toplevel; $(MAKE) clean)
	(cd common; $(MAKE) clean)
	(cd efuns; $(MAKE) clean)
	(cd gwml; $(MAKE) clean)
	(cd toolkit; $(MAKE) clean)
	(cd ocamlsrc/version/include; $(MAKE) clean)

fullclean: clean
	rm -f .cache Makefile.config
	(cd ocamlsrc; rm -f asmcomp config tools utils driver toplevel version asmcomp ocamlc boot ocamlopt bytecomp compat_comp.ml parsing byterun stdlib typing .depend)
	  rm -f inliner/perf/.depend common/.depend inliner/perf/Moretest/.depend concur/.depend inliner/tests/.depend concur/nothreads/.depend concur/threads/.depend  toolkit/.depend dynlink/.depend toolkit/examples/.depend efuns/.depend  toplevel/.depend gwml/.depend  xlib/.depend inliner/.depend  xlib/examples/.depend
	rm -f efuns/common/compat_run.ml efuns/dynlink/dynlink.mli efuns/toplevel/dynlink.mli

SOURCES=/usr/src/redhat/SOURCES/efuns-$(RELEASE)

rpm.src:
	rm -rf $(SOURCES)
	cp -dpR . $(SOURCES)

rpm.clean:
	(cd $(SOURCES);$(MAKE) fullclean;rm -rf `find . -name CVS -print`)

rpm.tar:
	(cd /usr/src/redhat/SOURCES; tar zcf efuns-$(VERSION).src.tar.gz efuns-$(RELEASE))
	
rpm.build: 
	rpm -ba efuns.spec

rpm: rpm.src rpm.clean rpm.tar rpm.build