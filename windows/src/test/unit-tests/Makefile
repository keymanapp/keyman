#
# Unit Tests Makefile.
#

# ----------------------------------------------------------------------

TARGETS=group-helper-rsp19902 kmcomp shared-data lexical-model-compiler

test:
    $(MAKE) -DTARGET=test $(TARGETS)

!include ..\..\Header.mak

# ----------------------------------------------------------------------

group-helper-rsp19902:
    cd $(ROOT)\src\test\unit-tests\group-helper-rsp19902
    $(MAKE) $(TARGET)

kmcomp:
    cd $(ROOT)\src\test\unit-tests\kmcomp
    $(MAKE) $(TARGET)

shared-data:
    cd $(ROOT)\src\test\unit-tests\shared-data
    $(MAKE) $(TARGET)

lexical-model-compiler:
    cd $(KEYMAN_ROOT)\developer\js

!ifdef GIT_BASH_FOR_KEYMAN
    $(GIT_BASH_FOR_KEYMAN) build.sh -test
!else
    start /wait ./build.sh -test
!endif


# ----------------------------------------------------------------------

!include ..\..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
