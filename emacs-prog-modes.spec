# -*- coding: utf-8; mode: rpm-spec -*-
# $Id: emacs-prog-modes.spec,v 1.1 2005/10/13 14:10:29 eugene Exp $

Version: 0.2
Release: alt1
Name: emacs-prog-modes
License: GPL
Group: Editors
Summary: Various programming packages for Emacs
Summary(ru_RU.UTF-8): Дополнительные пакеты Emacs для работы с исходными текстами программ
Requires: emacs-X11 deroff

Packager: Emacs Maintainers Team <emacs@packages.altlinux.org>

Source: %name.tar.gz
Source1: emacs-mode-php-site-start.el
Source2: emacs-c-mode-addons-site-start.el
Source3: emacs-mode-eiffel-site-start.el
Source4: emacs-mode-postscript-site-start.el
Source5: emacs-mode-rexx-site-start.el
Source6: emacs-mode-rpm-site-start.el
Source8: emacs-mode-vrml-site-start.el
Source9: emacs-mode-xbase-site-start.el

BuildArch: noarch

# Automatically added by buildreq on Thu Oct 13 2005
BuildRequires: emacs-cedet emacs-common emacs-elib emacs-leim fontconfig freetype2 xorg-x11-locales

%description
Various programming packages for Emacs, including packages for editing
programms on C, Scheme, Fortran and others.

%description -l ru_RU.UTF-8
Дополнительные пакеты Emacs для работы с исходными текстами программ на
языках С, Fortran, Scheme и других, а также различные вспомогательные
режимы для редактирования файлов Autoconf/Automake, отладки и многого
другого.

%prep
%setup -q -n %name

%build
for i in *.el ; do
  emacs -batch --eval "(progn (add-to-list 'load-path \".\") (byte-compile-file \"$i\"))"
done

%install
%__mkdir_p %buildroot%_emacslispdir/
%__install -m 644 *.el* %buildroot%_emacslispdir/
%__mkdir_p %buildroot%_datadir/emacs/etc/prog-modes/
%__install -m 755 *.sh %buildroot%_datadir/emacs/etc/prog-modes/
%__install -m 644 c_synopsis_list %buildroot%_datadir/emacs/etc/prog-modes/
%__install -pD -m0644 %SOURCE1 %buildroot%_sysconfdir/emacs/site-start.d/php.el
%__install -pD -m0644 %SOURCE2 %buildroot%_sysconfdir/emacs/site-start.d/c-mode-addons.el
%__install -pD -m0644 %SOURCE3 %buildroot%_sysconfdir/emacs/site-start.d/eiffel.el
%__install -pD -m0644 %SOURCE4 %buildroot%_sysconfdir/emacs/site-start.d/postscript.el
%__install -pD -m0644 %SOURCE5 %buildroot%_sysconfdir/emacs/site-start.d/rexx.el
%__install -pD -m0644 %SOURCE6 %buildroot%_sysconfdir/emacs/site-start.d/rpm.el
%__install -pD -m0644 %SOURCE8 %buildroot%_sysconfdir/emacs/site-start.d/vrml.el
%__install -pD -m0644 %SOURCE9 %buildroot%_sysconfdir/emacs/site-start.d/xbase.el

%files
%doc emacs-prog-modes-list.txt
%_emacslispdir/*.el*
%_datadir/emacs/etc/prog-modes/
%config(noreplace) %_sysconfdir/emacs/site-start.d/*


# TODO
# 1. Убрать после появления в сизиф emacs22 antlr-mode, cperl-mode, delphi
#    flymake


%changelog
* Wed Oct 12 2005 Eugene Vlasov <eugvv@altlinux.ru> 0.2-alt1
- Updated clearcase, constants, eiffel, flymake, javascript-mode,
  mode-compile, php-mode, quack, rpm-spec-mode, verilog-mode
- Added c-eldoc, compile-, compile+, sql-indent
- Removed eshell-bash, http-mode, p4, pydoc, teco
- Fixed build requires

* Wed Jul 14 2004 AEN <aen@altlinux.ru> 0.1-alt13
- NMU: rebuild in Master 2.4 environment 

* Thu May 06 2004 Ott Alex <ott@altlinux.ru> 0.1-alt12
- Added flymake package
- Added codemetric package
- Added different minor modes for work with CVS

* Tue Jan 27 2004 Ott Alex <ott@altlinux.ru> 0.1-alt11
- Fixing startup scripts

* Mon Jan 26 2004 Ott Alex <ott@altlinux.ru> 0.1-alt10
- Fixing startup scripts

* Tue Dec 16 2003 Ott Alex <ott@altlinux.ru> 0.1-alt9
- Remove ruby-mode, that conflicts with emacs-ruby-mode package

* Sat Dec 13 2003 Ott Alex <ott@altlinux.ru> 0.1-alt8
- New version of php-mode 
- Move antlr-mode from emacs-common

* Sun Nov 30 2003 Ott Alex <ott@altlinux.ru> 0.1-alt7
- add php-mode from emacs-common to package
- adding more site-start scripts

* Tue Oct 28 2003 Ott Alex <ott@altlinux.ru> 0.1-alt6
- Move pov-im file to emacs-pov-mode

* Sun Oct 26 2003 Ott Alex <ott@altlinux.ru> 0.1-alt5
- Adding new packages

* Tue Sep 23 2003 Ott Alex <ott@altlinux.ru> 0.1-alt4
- New snapshot

* Fri Jan 17 2003 Ott Alex <ott@altlinux.ru> 0.1-alt3
- Fixing spec-file

* Mon Jan 13 2003 Ott Alex <ott@altlinux.ru> 0.1-alt2
- New release of files

* Tue Dec 24 2002 Ott Alex <ott@altlinux.ru> 0.1-alt1
- Initial build
