#
# Keyman ext Makefile - 3rd party components
#

NOTARGET_SIGNCODE=yes

!ifdef NODELPHI
TARGETS=.virtual
!else
TARGETS=common jedi cef4delphi
!endif

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
