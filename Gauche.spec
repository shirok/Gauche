# Spec file to build Gauche RPM package
# $Id: Gauche.spec,v 1.7 2002-06-15 03:16:45 shirok Exp $
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

%define version  0.5.6
%define encoding eucjp

Summary: Scheme script interpreter with multibyte character handling
Name: Gauche
Version: %{version}
Release: 1
Source: Gauche-%{version}.tgz
Copyright: BSD
Group: Development/Languages
Packager: Shiro Kawai (shiro@acm.org)
Buildroot: %{_tmppath}/rpm
URL: http://www.shiro.dreamhost.com/scheme/gauche/
#Prefix: /usr
Requires: gdbm >= 1.8.0

%description
Gauche is a Scheme interpreter conforming Revised^5 Report on
Algorithmic Language Scheme.  It is designed for rapid development
of daily tools like system management and text processing.
It can handle multibyte character strings natively.

%package %{encoding}
Summary: Scheme script interpreter with multibyte character handling
Group: Development/Languages
Provides: Gauche
%description %{encoding}
Gauche is a Scheme interpreter conforming Revised^5 Report on
Algorithmic Language Scheme.  It is designed for rapid development
of daily tools like system management and text processing.
It can handle multibyte character strings natively.
This package is compiled with %{encoding} as the native character encoding.

%prep
%setup

%build
%ifarch i386
OPTFLAG="I686OPT="
%endif
./configure --prefix=/usr --enable-multibyte=%{encoding}
make ${OPTFLAG}

%install
# These dirs are not cleared after rpm -ba --clean.   To ensure clean
# install, we remove them.
rm -rf ${RPM_BUILD_ROOT}/usr/lib/gauche
rm -rf ${RPM_BUILD_ROOT}/usr/share/gauche
rm -rf ${RPM_BUILD_ROOT}/usr/share/man/man1
mkdir -p ${RPM_BUILD_ROOT}/usr
make prefix=${RPM_BUILD_ROOT}/usr install-rpm
make prefix=${RPM_BUILD_ROOT}/usr install-doc

%clean

%post

%post %{encoding}
# creates slib catalog, if possible.
/usr/bin/gosh -u slib -e "(require 'logical)" -e "(exit 0)" > /dev/null 2>&1 || echo

%files

%files %{encoding}
%defattr(-,root,root)
%doc COPYING ChangeLog INSTALL INSTALL.eucjp Gauche.spec
/usr/bin/gosh
/usr/bin/gauche-config
/usr/lib/gauche/
/usr/share/gauche/
/usr/share/man/man1/

%changelog
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

