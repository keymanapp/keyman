#
# Keyman ext Makefile - 3rd party components
#

NOTARGET_SIGNCODE=yes

!ifdef NODELPHI
TARGETS=.virtual
!else
TARGETS=dcpcrypt #jedi cef4delphi
!endif

# jedi has no packages

CLEANS=

!include ..\Header.mak

# ----------------------------------------------------------------------

cef4delphi: .virtual
    cd $(COMMON_ROOT)\ext\cef4delphi
    $(MAKE) $(TARGET)

dcpcrypt: .virtual
    cd $(COMMON_ROOT)\ext\dcpcrypt
    $(MAKE) $(TARGET)

jedi: .virtual
    cd $(COMMON_ROOT)\ext\jedi
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
