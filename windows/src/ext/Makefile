#
# Keyman ext Makefile - 3rd party components
#

NOTARGET_SIGNCODE=yes

TARGETS=dcpcrypt jedi mbcolor regexpr scfontcombobox cef4delphi

# jedi, regexpr have no packages

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

regexpr: .virtual
    cd $(ROOT)\src\ext\regexpr
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
