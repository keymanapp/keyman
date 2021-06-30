#
# Keyman buildtools Makefile
#

NOTARGET_SIGNCODE=yes

TARGETS=devtools sentrytool tds2dbg mkver \
  buildunidata build_standards_data buildpkg test-klog delphiprojectmanager
CLEANS=inst clean-buildtools

!include ..\Header.mak

# ----------------------------------------------------------------------

help:
    cd $(ROOT)\src\buildtools\help
    $(MAKE) $(TARGET)

build_standards_data:
    cd $(ROOT)\src\buildtools\build_standards_data
    $(MAKE) $(TARGET)

sentrytool:
    cd $(ROOT)\src\buildtools\sentrytool
    $(MAKE) $(TARGET)

buildunidata:
    cd $(ROOT)\src\buildtools\buildunidata
    $(MAKE) $(TARGET)

delphiprojectmanager:
    cd $(ROOT)\src\buildtools\delphiprojectmanager
    $(MAKE) $(TARGET)

devtools:
    cd $(ROOT)\src\buildtools\devtools
    $(MAKE) $(TARGET)

mkver:
    cd $(ROOT)\src\buildtools\mkver
    $(MAKE) $(TARGET)

inst:
    cd $(ROOT)\src\buildtools\inst
    $(MAKE) $(TARGET)

buildpkg:
    cd $(ROOT)\src\buildtools\buildpkg
    $(MAKE) $(TARGET)

test-klog:
    cd $(ROOT)\src\buildtools\test-klog
    $(MAKE) $(TARGET)

tds2dbg:
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
