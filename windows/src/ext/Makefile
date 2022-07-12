#
# Keyman ext Makefile - 3rd party components
#

NOTARGET_SIGNCODE=yes

!ifdef NODELPHI
TARGETS=.virtual
!else
TARGETS=common
!endif

CLEANS=

!include ..\Header.mak

# ----------------------------------------------------------------------

common: .virtual
    cd $(COMMON_ROOT)\ext
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

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
