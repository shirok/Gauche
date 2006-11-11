# Spec file to build Gauche RPM package
# $Id: Gauche.spec,v 1.45 2006-11-11 20:45:17 shirok Exp $
#
# In order to build different encoding-specific packages (like
# Gauche-euc-jp, etc) from a single source rpm, the actual package
# is created as a subpackage.  The command
# 
#   rpm -ba Gauche.spec
#
# builds three packages:
#    Gauche-VERS.ARCH.rpm         ;; dummy package (no use; discard it)
#    Gauche-ENC-VERS.ARCH.rpm     ;; binary package with encoding ENC
#    Gauche-VERS.src.rpm          ;; source package

%define version  0.8.8
%define encoding utf8
%define threads  pthreads

Summary: Scheme script interpreter with multibyte character handling
Name: Gauche
Version: %{version}
Release: 1
Source: Gauche-%{version}.tgz
License: revised BSD
Group: Development/Languages
Packager: Shiro Kawai (shiro@acm.org)
Buildroot: %{_tmppath}/rpm
URL: http://practical-scheme.net/gauche/
#Prefix: /usr

%description
Gauche is a Scheme interpreter conforming Revised^5 Report on
Algorithmic Language Scheme.  It is designed for rapid development
of daily tools like system management and text processing.
It can handle multibyte character strings natively.

%package %{encoding}
Summary: Scheme script interpreter with multibyte character handling
Group: Development/Languages
Provides: Gauche libgauche.so
License: revised BSD
Requires: Gauche-common
%description %{encoding}
Gauche is a Scheme interpreter conforming Revised^5 Report on
Algorithmic Language Scheme.  It is designed for rapid development
of daily tools like system management and text processing.
It can handle multibyte character strings natively.
This package is compiled with %{encoding} as the native character encoding.

%package common
Summary: Scheme script interpreter with multibyte character handling
Group: Development/Languages
License: revised BSD
%description common
Gauche is a Scheme interpreter conforming Revised^5 Report on
Algorithmic Language Scheme.  It is designed for rapid development
of daily tools like system management and text processing.
It can handle multibyte character strings natively.
This package includes common part that is independent from any
native character encoding.  You need either Gauche-eucjp or Gauche-utf8
package as well.

%package gdbm-%{encoding}
Summary: gdbm binding for Gauche Scheme system
Group: Development/Languages
License: GPL
Provides: Gauche-gdbm
Requires: gdbm >= 1.8.0, Gauche-%{encoding}
%description gdbm-%{encoding}
This package adds gdbm binding to the Gauche Scheme system.

%prep
%setup

%build
./configure --prefix=/usr --mandir='${prefix}/share/man' --infodir='${prefix}/share/info' --enable-threads=%{threads} --enable-multibyte=%{encoding}
%ifarch i386
make OPTFLAGS="-fomit-frame-pointer"
%else
make
%endif


%install
# These dirs are not cleared after rpm -ba --clean.   To ensure clean
# install, we remove them.
rm -rf ${RPM_BUILD_ROOT}/usr/lib/gauche
rm -rf ${RPM_BUILD_ROOT}/usr/share/gauche
rm -rf ${RPM_BUILD_ROOT}/usr/share/man/man1
mkdir -p ${RPM_BUILD_ROOT}/usr
make DESTDIR=${RPM_BUILD_ROOT}/ install-pkg
make DESTDIR=${RPM_BUILD_ROOT}/ install-doc

%clean

%post

%post %{encoding}
# creates slib catalog, if possible.
/usr/bin/gosh -u slib -e "(require 'logical)" -e "(exit 0)" > /dev/null 2>&1 || echo

%files common -f rpmfiles-common.txt
%defattr(-,root,root)
%doc COPYING ChangeLog INSTALL INSTALL.eucjp Gauche.spec
/usr/share/info/
/usr/share/man/man1/
/usr/share/gauche/site
/usr/share/aclocal/gauche.m4

%files %{encoding} -f rpmfiles-encoding.txt
%defattr(-,root,root)
/usr/bin/gosh
/usr/bin/gauche-config
/usr/bin/gauche-cesconv
/usr/bin/gauche-install
/usr/bin/gauche-package
#/usr/lib/libgauche.a
/usr/lib/libgauche.so
/usr/lib/libgauche.so.0
/usr/lib/libgauche.so.%{version}
/usr/lib/gauche/site/

%files gdbm-%{encoding} -f rpmfiles-gdbm.txt
%defattr(-,root,root)

%changelog
* Tue Nov 11 2006 Shiro Kawai
- Gauche release 0.8.8.

* Tue Apr 12 2006 Shiro Kawai
- Gauche release 0.8.7.

* Tue Nov  4 2005 Shiro Kawai
- Gauche release 0.8.6.

* Tue Jun 30 2005 Shiro Kawai
- Gauche release 0.8.5.

* Tue May 31 2005 Shiro Kawai
- Gauche release 0.8.4.

* Thu Dec  2 2004 Shiro Kawai
- Gauche release 0.8.3.

* Mon Nov 29 2004 Shiro Kawai
- Gauche release 0.8.2.

* Tue Aug  2 2004 Shiro Kawai
- Gauche release 0.8.1.

* Tue May 22 2004 Shiro Kawai
- Gauche release 0.8.

* Tue Feb 26 2004 Shiro Kawai
- Gauche release 0.7.4.2.

* Tue Feb  4 2004 Shiro Kawai
- Gauche release 0.7.4.1.

* Tue Feb  3 2004 Shiro Kawai
- Gauche release 0.7.4.

* Tue Dec 16 2003 Shiro Kawai
- Gauche release 0.7.3.

* Wed Oct 22 2003 Shiro Kawai
- Fix gdbm package license

* Tue Oct 21 2003 Shiro Kawai
- include aclocal/gauche.m4 in common rpm

* Sat Oct  4 2003 Shiro Kawai
- Gauche release 0.7.2.
Splitted into common, encoding-dependent part, and gdbm package.

* Wed Jul 23 2003 Shiro Kawai
- Gauche release 0.7.1

* Fri May 30 2003 Shiro Kawai
- Gauche release 0.7

* Sun Mar 30 2003 Shiro Kawai
- Gauche release 0.6.8

* Thu Feb  7 2003 Shiro Kawai
- Gauche release 0.6.7.1

* Thu Feb  6 2003 Shiro Kawai
- Gauche release 0.6.7

* Fri Dec 14 2002 Shiro Kawai
- Gauche release 0.6.6

* Fri Nov 15 2002 Shiro Kawai
- Gauche release 0.6.5

* Mon Oct 14 2002 Shiro Kawai
- Gauche release 0.6.4

* Thu Sep 22 2002 Shiro Kawai
- Gauche release 0.6.3

* Thu Sep  2 2002 Shiro Kawai
- Gauche release 0.6.2

* Thu Jul 31 2002 Shiro Kawai
- Gauche release 0.6.1

* Thu Jul 18 2002 Shiro Kawai
- Gauche release 0.6

* Sun Jun 30 2002 Shiro Kawai
- Gauche release 0.5.7

* Fri Jun 14 2002 Shiro Kawai
- Gauche release 0.5.6

* Mon May 27 2002 Shiro Kawai
- Gauche release 0.5.5

* Sat May  5 2002 Shiro Kawai
- Gauche release 0.5.4

* Thu Apr 15 2002 Shiro Kawai
- Gauche release 0.5.3

* Thu Mar  7 2002 Shiro Kawai
- first package release

