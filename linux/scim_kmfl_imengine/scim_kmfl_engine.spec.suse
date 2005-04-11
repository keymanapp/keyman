# created by RPM Builder for Anjuta, v0.1.2
# http://arpmbuilder.sourceforge.net
# Wed Feb 25 12:58:05 2004

%define _prefix	/usr
%define _unpackaged_files_terminate_build 0

Summary:         %{name}
Name:            scim_kmfl_imengine
Version:         0.3
Release:         1suse
Vendor:          SIL <doug_rintoul@sil.org>
Packager:        Doug Rintoul <doug_rintoul@sil.org>
Group:           Applications/System
License:         GPL
Source0:         %{name}-%{version}.suse.tar.gz
# Url:             (none)
BuildRoot:       /var/tmp/scim_kmfl_imengine
BuildArch:       i586
Requires:        libkmfl
Buildrequires:   libkmfl-devel
# Conflicts:       (none)
# Provides:        (none)
# Obsoletes:       (none)

%description 
KMFL imengine for SCIM

%package devel
Summary:         Static libraries and headers for %{name}
Group:           Development/Libraries
# Requires:        (none)
# Buildrequires:   (none)
# Conflicts:       (none)
# Provides:        (none)
# Obsoletes:       (none)

%description devel
KMFL imengine for SCIM

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
	rm -f $RPM_BUILD_ROOT%{_prefix}/doc/scim_kmfl_imengine/$doc;
done;

%clean
[ -n "$RPM_BUILD_ROOT" -a "$RPM_BUILD_ROOT" != "/" ] && rm -rf $RPM_BUILD_ROOT

%post
/sbin/ldconfig
/opt/gnome/bin/gtk-query-immodules-2.0 > %{_sysconfdir}/opt/gnome/gtk-2.0/gtk.immodules

%postun
/sbin/ldconfig
/opt/gnome/bin/gtk-query-immodules-2.0 > %{_sysconfdir}/opt/gnome/gtk-2.0/gtk.immodules

%files 
%defattr(-,root,root)
%{_libdir}/scim-1.0/1.0.0/IMEngine/kmfl.so
%{_libdir}/scim-1.0/1.0.0/SetupUI/kmfl_imengine_setup.so
/opt/gnome/lib/gtk-2.0/immodules/im-xim-kmfl.so

%doc AUTHORS COPYING ChangeLog README INSTALL NEWS TODO

%files devel
%defattr(-,root,root)
%{_libdir}/scim-1.0/1.0.0/IMEngine/kmfl.la
%{_libdir}/scim-1.0/1.0.0/IMEngine/kmfl.a
%{_libdir}/scim-1.0/1.0.0/SetupUI/kmfl_imengine_setup.a
%{_libdir}/scim-1.0/1.0.0/SetupUI/kmfl_imengine_setup.la

