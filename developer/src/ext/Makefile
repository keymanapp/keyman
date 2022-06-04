#
# Keyman Developer ext Makefile - 3rd party components
#

NOTARGET_SIGNCODE=yes

!ifdef NODELPHI
TARGETS=.virtual
!else
TARGETS=browse4folder mbcolor common
!endif

CLEANS=

!include ..\Header.mak

# ----------------------------------------------------------------------

browse4folder: .virtual
    cd $(DEVELOPER_ROOT)\src\ext\browse4folder
    $(MAKE) $(TARGET)

mbcolor: .virtual
    cd $(DEVELOPER_ROOT)\src\ext\mbcolor
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

common: .virtual
    cd $(COMMON_ROOT)\ext
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
