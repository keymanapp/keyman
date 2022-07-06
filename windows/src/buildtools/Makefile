#
# Keyman buildtools Makefile
#

NOTARGET_SIGNCODE=yes

!ifdef NODELPHI
TARGETS=.virtual
!else
TARGETS=common buildpkg
!endif

CLEANS=clean-buildtools

!include ..\Header.mak

# ----------------------------------------------------------------------

common: .virtual
    cd $(KEYMAN_ROOT)\common\windows\delphi\tools
    $(MAKE) $(TARGET)

buildpkg: .virtual
    cd $(ROOT)\src\buildtools\buildpkg
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

clean-buildtools:
    cd $(ROOT)\src\buildtools
    -del version.txt

!include ..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
