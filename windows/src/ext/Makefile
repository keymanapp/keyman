#
# Keyman ext Makefile - 3rd party components
#

NOTARGET_SIGNCODE=yes

!ifdef NODELPHI
TARGETS=.virtual
!else
TARGETS=common jedi scfontcombobox cef4delphi
!endif

# jedi has no packages

CLEANS=

!include ..\Header.mak

# ----------------------------------------------------------------------

common: .virtual
    cd $(COMMON_ROOT)\ext
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

cef4delphi: .virtual
    cd $(ROOT)\src\ext\cef4delphi
    $(MAKE) $(TARGET)

jedi: .virtual
    cd $(ROOT)\src\ext\jedi
    $(MAKE) $(TARGET)

vclzip: .virtual
    cd $(ROOT)\src\ext\vclzip
    $(MAKE) $(TARGET)

scfontcombobox: .virtual
    cd $(ROOT)\src\ext\scfontcombobox
    $(MAKE) $(TARGET)

#
#
#

reset:
    $(DEVTOOLS) -rp
    $(DEVTOOLS) -ri

!include ..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
