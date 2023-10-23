#
# Keyman buildtools Makefile
#

NOTARGET_SIGNCODE=yes

!ifdef NODELPHI
TARGETS=.virtual
!else
TARGETS=common
!endif

CLEANS=clean-buildtools

!include ..\Header.mak

# ----------------------------------------------------------------------

common: .virtual
    cd $(KEYMAN_ROOT)\common\windows\delphi\tools
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

clean-buildtools:
    rem no action

!include ..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
