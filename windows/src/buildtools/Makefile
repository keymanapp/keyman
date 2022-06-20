#
# Keyman buildtools Makefile
#

NOTARGET_SIGNCODE=yes

!ifdef NODELPHI
TARGETS=.virtual
!else
TARGETS=common devtools sentrytool buildpkg test-klog
!endif

CLEANS=clean-buildtools

!include ..\Header.mak

# ----------------------------------------------------------------------

common: .virtual
    cd $(KEYMAN_ROOT)\common\windows\delphi\tools
    $(MAKE) $(TARGET)

sentrytool: .virtual
    cd $(ROOT)\src\buildtools\sentrytool
    $(MAKE) $(TARGET)

devtools: .virtual
!ifdef NODELPHI
    echo Skipping devtools
!else
    cd $(ROOT)\src\buildtools\devtools
    $(MAKE) $(TARGET)
!endif

buildpkg: .virtual
    cd $(ROOT)\src\buildtools\buildpkg
    $(MAKE) $(TARGET)

test-klog: .virtual
    cd $(ROOT)\src\buildtools\test-klog
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

clean-buildtools:
    cd $(ROOT)\src\buildtools
    -del version.txt

!include ..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
