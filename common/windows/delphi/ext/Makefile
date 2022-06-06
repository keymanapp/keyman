#
# Keyman ext Makefile - 3rd party components
#

NOTARGET_SIGNCODE=yes

!ifdef NODELPHI
TARGETS=.virtual
!else
TARGETS=cef4delphi dcpcrypt jwa tds2dbg
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

jwa: .virtual
    cd $(COMMON_ROOT)\ext\jwa
    $(MAKE) $(TARGET)

tds2dbg: .virtual
    cd $(COMMON_ROOT)\ext\tds2dbg
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
