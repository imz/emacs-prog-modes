Version: 0.1
Release: alt6
Name: emacs-prog-modes
License: GPL
Group: Editors
Summary: Various programming packages for Emacs
Summary(ru_RU.KOI8-R): Дополнительные пакеты Emacs для работы с исходными текстами программ
Requires: emacs-common deroff

Source: %name.tar.gz

BuildArch: noarch

# Automatically added by buildreq on Tue Dec 24 2002
BuildRequires: emacs-common python22

%description
Various programming packages for Emacs, including packages for editing
programms on C, Scheme, Fortran, Ruby and others.

%description -l ru_RU.KOI8-R
Дополнительные пакеты Emacs для работы с исходными текстами программ на
языках С, Fortran, Scheme, Ruby и других, а также различные вспомогательные
режимы для редактирования файлов Autoconf/Automake, отладки и многого
другого.

%prep
%setup -n %name

%build
for i in *.el ; do
	emacs -batch --eval "(progn
	(setq load-path (append (list \".\")  load-path))
	(byte-compile-file \"$i\"))"
done

%install
mkdir -p %buildroot%_emacslispdir/
install -m 644 *.el* %buildroot%_emacslispdir/
mkdir -p %buildroot%_datadir/emacs/etc/prog-modes/
install -m 755 *.sh *.py %buildroot%_datadir/emacs/etc/prog-modes/
install -m 644 c_synopsis_list %buildroot%_datadir/emacs/etc/prog-modes/

%files
%doc emacs-prog-modes-list.txt
%_emacslispdir/*.el*
%_datadir/emacs/etc/prog-modes/

%changelog
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
