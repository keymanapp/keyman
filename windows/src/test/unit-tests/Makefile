#
# Unit Tests Makefile.
#

# ----------------------------------------------------------------------

TARGETS=group-helper-rsp19902 kmcomp

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

# ----------------------------------------------------------------------

!include ..\..\Target.mak
    
# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
