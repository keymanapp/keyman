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
Name:            kmflcomp
Version:         0.9.9
Release:         2%{disttag}%{distver}
Vendor:          SIL <doug_rintoul@sil.org>
Packager:        Doug Rintoul <doug_rintoul@sil.org>
Group:           Applications/System
License:         GPL
Source0:         %{name}-%{version}.tar.gz
# Url:             (none)
BuildRoot:       /var/tmp/kmflcomp
%if %is_opensuse
Buildrequires:	xorg-x11-devel
%endif
# Requires:        (none)
# Conflicts:       (none)
# Provides:        (none)
# Obsoletes:       (none)

%description 
Compile Keyman-style keyboard layout files to a binary format for use by the KMFL keystroke interpreter.

%package devel
Summary:         Development libraries and headers to use %{name} in an application
Group:           Development/Libraries
# Requires:        (none)
# Buildrequires:   (none)
# Conflicts:       (none)
# Provides:        (none)
# Obsoletes:       (none)


%description devel
Library to compile Keyman-style keyboard layout files 

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
	rm -f $RPM_BUILD_ROOT%{_prefix}/doc/kmflcomp/$doc;
done;

%clean
[ -n "$RPM_BUILD_ROOT" -a "$RPM_BUILD_ROOT" != "/" ] && rm -rf $RPM_BUILD_ROOT

%files 
%defattr(-,root,root)
%{_bindir}/kmflcomp
%{_libdir}/libkmflcomp.so.0.0.0
%{_libdir}/libkmflcomp.so.0


%doc AUTHORS COPYING ChangeLog README INSTALL NEWS TODO

%files devel
%defattr(-,root,root)
%{_includedir}/kmfl/kmfl.h
%{_includedir}/kmfl/kmflcomp.h
%{_includedir}/kmfl/kmflutfconv.h
%{_libdir}/libkmflcomp.la
%{_libdir}/libkmflcomp.a
%{_libdir}/libkmflcomp.so

