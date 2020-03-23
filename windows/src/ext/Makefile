#
# Keyman ext Makefile - 3rd party components
#

NOTARGET_SIGNCODE=yes

TARGETS=dcpcrypt jedi mbcolor regexpr scfontcombobox cef4delphi

# jedi, regexpr have no packages

CLEANS=

!include ..\Header.mak

# ----------------------------------------------------------------------

cef4delphi:
    cd $(ROOT)\src\ext\cef4delphi
    $(MAKE) $(TARGET)
    
dcpcrypt:
    cd $(ROOT)\src\ext\dcpcrypt
    $(MAKE) $(TARGET)

jedi:
    cd $(ROOT)\src\ext\jedi
    $(MAKE) $(TARGET)

mbcolor:
    cd $(ROOT)\src\ext\mbcolor
    $(MAKE) $(TARGET)

regexpr:
    cd $(ROOT)\src\ext\regexpr
    $(MAKE) $(TARGET)

vclzip:
    cd $(ROOT)\src\ext\vclzip
    $(MAKE) $(TARGET)

scfontcombobox:
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
