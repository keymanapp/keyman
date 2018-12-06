# created by RPM Builder for Anjuta, v0.1.2
# http://arpmbuilder.sourceforge.net
# Wed Feb 25 12:14:17 2004

%define _prefix	/usr

%define is_mandrake %(test -e /etc/mandrake-release && echo 1 || echo 0)
%define is_mandriva %(test -e /etc/mandriva-release && echo 1 || echo 0)
%define is_suse %(test -e /etc/SuSE-release && echo 1 || echo 0)
%define is_opensuse %(grep open /etc/SuSE-release 2> /dev/null 1>/dev/null && echo 1 || echo 0)
%define is_fedora %(test -e /etc/fedora-release && echo 1 || echo 0)

%if %is_mandrake
%define dist mandrake
%define disttag mdk
%endif
%if %is_mandriva
%define dist mandriva
%define disttag mdk
%endif
%if %is_suse
%define dist suse
%define disttag suse
%define kde_path /opt/kde3
%endif
%if %is_opensuse
%define dist openSUSE
%define disttag openSUSE
%define kde_path /opt/kde3
%endif
%if %is_fedora
%define dist fedora
%define disttag fc
%endif

%define distver %(release="`rpm -q --queryformat='%{VERSION}' %{dist}-release 2> /dev/null | tr . : | sed s/://g`" ; if test $? != 0 ; then release="" ; fi ; echo "$release")

Summary:         %{name}
Name:            libkmfl
Version:         0.9.12
Release:         1%{disttag}%{distver}
Vendor:          SIL <doug_rintoul@sil.org>
Packager:        Doug Rintoul <doug_rintoul@sil.org>
Group:           User Interface/X
License:         GPL
Source0:         %{name}-%{version}.tar.gz
# Url:             (none)
BuildRoot:       /var/tmp/libkmfl
Requires:        kmflcomp
Buildrequires:   kmflcomp-devel
# Conflicts:       (none)
# Provides:        (none)
# Obsoletes:       (none)

%description 
Keystroke interpreter for KMFL

%package devel
Summary:         Development libraries and headers to use %{name} in an application
Group:           Development/Libraries
Requires:        kmflcomp
Buildrequires:   kmflcomp-devel
# Conflicts:       (none)
# Provides:        (none)
# Obsoletes:       (none)

%description devel
Keystroke interpreter for KMFL

%prep
%setup -q

%build
[ ! -f Makefile ] || make distclean
%configure
make

%install
[ -n "$RPM_BUILD_ROOT" -a "$RPM_BUILD_ROOT" != "/" ] && rm -rf $RPM_BUILD_ROOT

%makeinstall
for doc in ABOUT-NLS AUTHORS README COPYING INSTALL NEWS TODO ChangeLog; do
	rm -f $RPM_BUILD_ROOT%{_prefix}/doc/libkmfl/$doc;
done;

%clean
[ -n "$RPM_BUILD_ROOT" -a "$RPM_BUILD_ROOT" != "/" ] && rm -rf $RPM_BUILD_ROOT

%files 
%defattr(-,root,root)
%{_libdir}/libkmfl.so.0.0.0
%{_libdir}/libkmfl.so.0

%doc AUTHORS COPYING ChangeLog README INSTALL NEWS TODO

%files devel
%defattr(-,root,root)
%{_includedir}/kmfl/libkmfl.h
%{_libdir}/libkmfl.la
%{_libdir}/libkmfl.a
%{_libdir}/libkmfl.so

