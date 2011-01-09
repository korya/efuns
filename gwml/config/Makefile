
CONFIG=cpp.cmo sendEvent.cmo image.cmo opttable.cmo stdconfig.cmo animate.cmo stdvirtual.cmo user.cmo stdmenus.cmo stdicon.cmo stdplacement.cmo themes.cmo stddeco.cmo appMgr.cmo modules.cmo fvwm.cmo wmaker.cmo afterstep.cmo twm_t.cmo twm_l.cmo twm_p.cmo twm.cmo group.cmo gnome.cmo undo.cmo dvroom.cmo stdgwmlrc.cmo gwmlrc.cmo configurator.cmo

TMPFILES=twm_p.mli twm_p.ml twm_l.ml fvwm.ml wmaker.ml cpp.ml afterstep.ml

all: $(TMPFILES) $(CONFIG)

OCAMLC=/usr/local/lib/xlib/3.01-/ocamlrun /usr/local/lib/xlib/3.01-/ocamlc
OCAMLDEP=ocamldep
INCLUDES=-I /usr/local/lib/gwml/3.01- -I /usr/local/lib/xlib/3.01-

include Makefile.rules
include .depend
