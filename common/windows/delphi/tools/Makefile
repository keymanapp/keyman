#
# Keyman common tools Makefile
#

NOTARGET_SIGNCODE=yes

!ifdef NODELPHI
TARGETS=.virtual
!else
TARGETS=build_standards_data buildunidata devtools sentrytool test-klog verify_signatures
!endif

CLEANS=clean-tools

!include ..\Header.mak

# ----------------------------------------------------------------------

build_standards_data: .virtual
    cd $(COMMON_ROOT)\tools\build_standards_data
    $(MAKE) $(TARGET)

sentrytool: .virtual
    cd $(COMMON_ROOT)\tools\sentrytool
    $(MAKE) $(TARGET)

buildunidata: .virtual
    cd $(COMMON_ROOT)\tools\buildunidata
    $(MAKE) $(TARGET)

verify_signatures: .virtual
    cd $(COMMON_ROOT)\tools\verify_signatures
    $(MAKE) $(TARGET)

devtools: .virtual
!ifdef NODELPHI
    echo Skipping devtools
!else
    cd $(COMMON_ROOT)\tools\devtools
    $(MAKE) $(TARGET)
!endif

test-klog: .virtual
    cd $(COMMON_ROOT)\tools\test-klog
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

clean-tools:
    cd $(COMMON_ROOT)\tools
    -del version.txt

!include ..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
