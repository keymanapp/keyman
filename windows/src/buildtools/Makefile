#
# Keyman buildtools Makefile
#

NOTARGET_SIGNCODE=yes

!ifdef NODELPHI
TARGETS=.virtual
!else
TARGETS=devtools sentrytool tds2dbg \
  buildunidata build_standards_data buildpkg test-klog delphiprojectmanager
!endif

CLEANS=clean-buildtools

!include ..\Header.mak

# ----------------------------------------------------------------------

build_standards_data: .virtual
    cd $(ROOT)\src\buildtools\build_standards_data
    $(MAKE) $(TARGET)

sentrytool: .virtual
    cd $(ROOT)\src\buildtools\sentrytool
    $(MAKE) $(TARGET)

buildunidata: .virtual
    cd $(ROOT)\src\buildtools\buildunidata
    $(MAKE) $(TARGET)

delphiprojectmanager: .virtual
    cd $(ROOT)\src\buildtools\delphiprojectmanager
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

tds2dbg: .virtual
    cd $(ROOT)\src\buildtools\tds2dbg
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

clean-buildtools:
    cd $(ROOT)\src\buildtools
    -del version.txt

!include ..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
