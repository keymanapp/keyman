dnl Process this file with autoconf to produce a configure script.
dnl Created by Anjuta - will be overwritten
dnl If you don't want it to overwrite it,
dnl 	Please disable it in the Anjuta project configuration

AC_INIT(scim-kmfl-imengine, 0.9.9)
AM_INIT_AUTOMAKE
AM_CONFIG_HEADER(config.h)

AC_CONFIG_MACRO_DIR([m4])

AC_USE_SYSTEM_EXTENSIONS
AC_ISC_POSIX
AC_SUBST(CFLAGS,$CFLAGS)
AC_SUBST(CXXFLAGS,$CXXFLAGS)
AC_PROG_CC
AC_PROG_CXX
AM_PROG_CC_STDC
AC_HEADER_STDC
AM_PROG_LIBTOOL
# AC_PROG_INTLTOOL([0.22])

PKG_CHECK_MODULES(GTK, gtk+-2.0 gdk-2.0,,exit)
GTK_LIBDIR=`$PKG_CONFIG --variable=libdir gtk+-2.0`
if test -z "$GTK_LIBDIR"; then
    GTK_LIBDIR="$libdir"
fi
AC_SUBST(GTK_LIBDIR)


AC_SUBST(GTK_LIBS)
AC_SUBST(GTK_CFLAGS)

dnl Checks for programs.
dnl Checks for libraries.
dnl Checks for header files.
AC_CHECK_HEADERS([langinfo.h libintl.h string.h dirent.h])

PKG_CHECK_MODULES(SCIM, scim,,exit)
dnl Checks for typedefs, structures, and compiler characteristics.
dnl Checks for library functions.
dnl Checks for Additional stuffs.
dnl Subst SCIM VERSION INFO
SCIM_MAJOR=`pkg-config --modversion scim | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
AC_SUBST(SCIM_MAJOR)

SCIM_MINOR=`pkg-config --modversion scim | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
AC_SUBST(SCIM_MINOR)

SCIM_BINARY_VERSION=`pkg-config --variable=scim_binary_version scim`
AC_SUBST(SCIM_BINARY_VERSION)

AC_SUBST(SCIM_LIBS)
AC_SUBST(SCIM_CFLAGS)

SCIM_DATADIR="$datadir/scim"
AC_SUBST(SCIM_DATADIR)

AM_GLIB_DEFINE_LOCALEDIR(GTK_LOCALEDIR)

GETTEXT_PACKAGE=scim_kmfl_imengine
AC_SUBST(GETTEXT_PACKAGE)
AC_DEFINE_UNQUOTED(GETTEXT_PACKAGE, "$GETTEXT_PACKAGE", [Package name for gettext])


dnl Languages which your application supports
ALL_LINGUAS=""
AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION(0.17)

#AM_GNU_GETTEXT_VERSION
#AM_GNU_GETTEXT

dnl Set PACKAGE_LOCALE_DIR in config.h.
if test "x${prefix}" = "xNONE"; then
  AC_DEFINE_UNQUOTED(PACKAGE_LOCALE_DIR, "${ac_default_prefix}/${DATADIRNAME}/locale")
else
  AC_DEFINE_UNQUOTED(PACKAGE_LOCALE_DIR, "${prefix}/${DATADIRNAME}/locale")
fi


dnl Set PACKAGE SOURCE DIR in config.h.
packagesrcdir=`cd $srcdir && pwd`

dnl Set PACKAGE PREFIX
if test "x${prefix}" = "xNONE"; then
  packageprefix=${ac_default_prefix}
else
  packageprefix=${prefix}
fi

dnl Set PACKAGE DATA & DOC DIR
packagedatadir=share/${PACKAGE}
packagedocdir=doc/${PACKAGE}

dnl Set PACKAGE DIRS in config.h.
packagepixmapsdir=${packagedatadir}/pixmaps
packagehelpdir=${packagedatadir}/help
packagemenudir=${packagedatadir}

dnl Subst PACKAGE_DATA_DIR.
NO_PREFIX_PACKAGE_DATA_DIR="${packagedatadir}"
AC_SUBST(NO_PREFIX_PACKAGE_DATA_DIR)
PACKAGE_DATA_DIR="${packageprefix}/${packagedatadir}"
AC_SUBST(PACKAGE_DATA_DIR)

dnl Subst PACKAGE_DOC_DIR.
NO_PREFIX_PACKAGE_DOC_DIR="${packagedocdir}"
AC_SUBST(NO_PREFIX_PACKAGE_DOC_DIR)
PACKAGE_DOC_DIR="${packageprefix}/${packagedocdir}"
AC_SUBST(PACKAGE_DOC_DIR)

dnl Subst PACKAGE_PIXMAPS_DIR.
NO_PREFIX_PACKAGE_PIXMAPS_DIR="${packagepixmapsdir}"
AC_SUBST(NO_PREFIX_PACKAGE_PIXMAPS_DIR)
PACKAGE_PIXMAPS_DIR="${packageprefix}/${packagepixmapsdir}"
AC_SUBST(PACKAGE_PIXMAPS_DIR)

dnl Subst PACKAGE_HELP_DIR.
NO_PREFIX_PACKAGE_HELP_DIR="${packagehelpdir}"
AC_SUBST(NO_PREFIX_PACKAGE_HELP_DIR)
PACKAGE_HELP_DIR="${packageprefix}/${packagehelpdir}"
AC_SUBST(PACKAGE_HELP_DIR)

dnl Subst PACKAGE_MENU_DIR.
NO_PREFIX_PACKAGE_MENU_DIR="${packagemenudir}"
AC_SUBST(NO_PREFIX_PACKAGE_MENU_DIR)
PACKAGE_MENU_DIR="${packageprefix}/${packagemenudir}"
AC_SUBST(PACKAGE_MENU_DIR)

AH_TEMPLATE([ENABLE_NLS])
AH_TEMPLATE([HAVE_CATGETS])
AH_TEMPLATE([HAVE_GETTEXT])
AH_TEMPLATE([HAVE_LC_MESSAGES])
AH_TEMPLATE([HAVE_STPCPY])
AH_TEMPLATE([HAVE_LIBSM])
AH_TEMPLATE([PACKAGE_LOCALE_DIR])
AH_TEMPLATE([PACKAGE_DOC_DIR])
AH_TEMPLATE([PACKAGE_DATA_DIR])
AH_TEMPLATE([PACKAGE_PIXMAPS_DIR])
AH_TEMPLATE([PACKAGE_HELP_DIR])
AH_TEMPLATE([PACKAGE_MENU_DIR])
AH_TEMPLATE([PACKAGE_SOURCE_DIR])
AH_TEMPLATE([GETTEXT_PACKAGE])

AC_DEFINE_UNQUOTED(PACKAGE_DATA_DIR, "${packageprefix}/${packagedatadir}")
AC_DEFINE_UNQUOTED(PACKAGE_DOC_DIR, "${packageprefix}/${packagedocdir}")
AC_DEFINE_UNQUOTED(PACKAGE_PIXMAPS_DIR, "${packageprefix}/${packagepixmapsdir}")
AC_DEFINE_UNQUOTED(PACKAGE_HELP_DIR, "${packageprefix}/${packagehelpdir}")
AC_DEFINE_UNQUOTED(PACKAGE_MENU_DIR, "${packageprefix}/${packagemenudir}")
AC_DEFINE_UNQUOTED(PACKAGE_SOURCE_DIR, "${packagesrcdir}")

AC_CONFIG_FILES([
Makefile
po/Makefile.in
src/Makefile
scim_kmfl_imengine_setup/Makefile
data/Makefile
data/icons/Makefile
])
AC_OUTPUT
