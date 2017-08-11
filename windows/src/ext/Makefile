#
# Keyman ext Makefile - 3rd party components
#

NOTARGET_SIGNCODE=yes

#
# tb2k plusmemo - encumbered, see encumbered_src
#

!ifdef USE_PLUSMEMO
ENCUMBERED_TARGETS=plusmemo
!ELSE
ENCUMBERED_TARGETS=
!ENDIF

TARGETS=$(ENCUMBERED_TARGETS) dcpcrypt embeddedwb jedi mbcolor regexpr scfontcombobox

# jedi, regexpr have no packages

CLEANS=

!include ..\Header.mak

# ----------------------------------------------------------------------

dcpcrypt:
    cd $(ROOT)\src\ext\dcpcrypt
    $(MAKE) $(TARGET)

embeddedwb:
    cd $(ROOT)\src\ext\embeddedwb
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
# Encumbered 3rd party packages
#

plusmemo: regexpr
    cd $(ENCUMBERED_EXT)\plusmemo
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
