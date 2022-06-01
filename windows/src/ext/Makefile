#
# Keyman ext Makefile - 3rd party components
#

NOTARGET_SIGNCODE=yes

!ifdef NODELPHI
TARGETS=.virtual
!else
TARGETS=dcpcrypt jedi mbcolor scfontcombobox cef4delphi
!endif

# jedi has no packages

CLEANS=

!include ..\Header.mak

# ----------------------------------------------------------------------

cef4delphi: .virtual
    cd $(ROOT)\src\ext\cef4delphi
    $(MAKE) $(TARGET)

dcpcrypt: .virtual
    cd $(ROOT)\src\ext\dcpcrypt
    $(MAKE) $(TARGET)

jedi: .virtual
    cd $(ROOT)\src\ext\jedi
    $(MAKE) $(TARGET)

mbcolor: .virtual
    cd $(ROOT)\src\ext\mbcolor
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
