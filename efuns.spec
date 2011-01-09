# Note that this is NOT a relocatable package
%define ver 0.17.0.0
%define rel 1
%define prefix /usr/local
%define srcdir /usr/src/cvs_checkouts/csl
%define efunsversion 017
%define ocaml 2.04
%define ocamllib ocaml-%{ocaml}
%define iprefix $RPM_BUILD_ROOT/%{prefix}
%define fullvers %{ocaml}-%{efunsversion}

Name: efuns
Summary: The Efuns package.
Version: %ver
Release: %rel
Source: ftp://pauillac.inria.fr/para/cdrom/ftp/unix/efuns/efuns-%{efunsversion}.src.tar.gz
Group: X11/Devel
BuildRoot: /tmp/efuns-%ver-build
Copyright: Copyright (C) 1998,1999,2000 Fabrice Le Fessant, INRIA.
Packager: Fabrice Le Fessant <Fabrice.Le_Fessant@inria.fr>
URL: http://pauillac.inria.fr/efuns
Distribution: efuns
Requires: imlib >= 1.9

Docdir: %{prefix}/doc

%description
The Efuns package contains:
 - The Efuns Emacs clone editor (an editor configurable in Ocaml).
 - The Gwml Generic Window-Manager (a wm configurable in Ocaml).
 - The wXtoolkit (a toolkit of widgets based on Xlib)
 - The Xlib library for Ocaml (complete emulation of the C Xlib library)
 - The Dynlink library for native code (allow you to dynamically load
      cmo files in native programs).
 - The Toplevel library (allow you to evaluate ocaml expressions inside
      any program -including native ones- as in the toplevel).

%changelog

%prep

%setup

%build
# This is what should be there:
#./configure -prefix %{prefix} -srcdir %{srcdir}
#make byte opt

# We don't build the version, instead we build the RPM binary from the one
# installed on my computer. However, if you want to create a RPM.

%install
# This is what should be there:
#make installroot=$RPM_BUILD_ROOT install installopt
#cd gwml; make installroot=$RPM_BUILD_ROOT install-icons install-ml install-static install-themes

# This is the fastest way:

mkdir -p %{iprefix}/bin/
for i in efuns gwml efuns_filebrowser efuns_client efuns_texbrowser xlib_config wX_config gwml_install; do
  cp %{prefix}/bin/$i %{iprefix}/bin/
done
mkdir -p  %{iprefix}/lib/efuns/
cp -r %{prefix}/lib/efuns/%{fullvers}  %{iprefix}/lib/efuns/%{fullvers} 
mkdir -p  %{iprefix}/lib/gwml/
cp -r %{prefix}/lib/gwml/%{fullvers}  %{iprefix}/lib/gwml/%{fullvers} 
mkdir -p  %{iprefix}/lib/xlib/
cp -r %{prefix}/lib/xlib/%{fullvers}  %{iprefix}/lib/xlib/%{fullvers} 
mkdir -p  %{iprefix}/share/
cp -r %{prefix}/share/GwML  %{iprefix}/share/GwML
rm -rf %{iprefix}/share/GwML/minouche

%clean
rm -rf $RPM_BUILD_ROOT

%post
# I should probably verify that the current version of Ocaml is OK.

%postun

%files
%defattr(-, root, root)

%{prefix}/bin/gwml
%{prefix}/bin/gwml_install
%{prefix}/bin/efuns
%{prefix}/bin/efuns_filebrowser
%{prefix}/bin/efuns_client
%{prefix}/bin/efuns_texbrowser
%{prefix}/bin/wX_config
%{prefix}/bin/xlib_config

%{prefix}/lib/xlib/%{fullvers}
%{prefix}/lib/gwml/%{fullvers}
%{prefix}/lib/efuns/%{fullvers}
%{prefix}/share/GwML

%doc INSTALL
%doc doc
%doc Changes
%doc efuns/Changes.efuns efuns/FAQ.efuns
%doc gwml/Changes.gwml gwml/FAQ.gwml
%doc toolkit/Changes.wXlib
%doc xlib/Changes.xlib
%doc toplevel/Changes.toplevel
%doc inliner/Changes.inliner
