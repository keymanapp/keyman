#
# Tests Makefile.
#

# ----------------------------------------------------------------------

TARGETS=test_i3633
CLEAN=test_i3633

test: test-manifest-exec
    $(MAKE) -DTARGET=test $(TARGETS)

# We want `test` to be the default target
!include ..\Header.mak

test-manifest-exec:
    cd ..
    $(MAKE) test-manifest
    cd test

# test_i3633: validate certificates and binary metadata on executables
# TODO: Move this to buildtools
test_i3633:
    cd $(ROOT)\src\test\test_i3633
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

!include ..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
