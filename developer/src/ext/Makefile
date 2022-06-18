#
# Keyman Developer ext Makefile - 3rd party components
#

NOTARGET_SIGNCODE=yes

!ifdef NODELPHI
TARGETS=.virtual
!else
TARGETS=browse4folder
!endif

CLEANS=

!include ..\Header.mak

# ----------------------------------------------------------------------

browse4folder: .virtual
    cd $(DEVELOPER_ROOT)\src\ext\browse4folder
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
