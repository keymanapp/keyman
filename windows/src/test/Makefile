#
# Tests Makefile.
#

# ----------------------------------------------------------------------

TARGETS=test_i3633 test_i3317
CLEAN=test_i3633 test_i3317

!include ..\Header.mak

test: test-manifest-exec
    $(MAKE) -DTARGET=test $(TARGETS)

test-manifest-exec:
    cd ..
    $(MAKE) test-manifest
    cd test

test_i3633:
    cd $(ROOT)\src\test\test_i3633
    $(MAKE) $(TARGET)

test_i3317:
    cd $(ROOT)\src\test\test_i3317
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

!include ..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
