#
# Unit Tests Makefile.
#

# ----------------------------------------------------------------------

TARGETS=package-info kmx-file-languages keyboard-js-info

test: 
    $(MAKE) -DTARGET=test $(TARGETS)
    
!include ..\Header.mak

# ----------------------------------------------------------------------

package-info:
    cd $(ROOT)\src\unit-tests\package-info
    $(MAKE) $(TARGET)

kmx-file-languages:
    cd $(ROOT)\src\unit-tests\kmx-file-languages
    $(MAKE) $(TARGET)

keyboard-js-info:
    cd $(ROOT)\src\unit-tests\keyboard-js-info
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

!include ..\Target.mak
    
# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
