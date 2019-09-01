#
# Unit Tests Makefile.
#

# ----------------------------------------------------------------------

TARGETS=group-helper-rsp19902 kmcomp shared-data

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
    npm run build
    npm test

# ----------------------------------------------------------------------

!include ..\..\Target.mak
    
# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
