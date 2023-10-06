#
# Tests Makefile.
#

# ----------------------------------------------------------------------

# TODO: both test-manifest-exec and verify_signatures are really part of buildtools

!ifdef NODELPHI
TARGETS=.virtual
!else
TARGETS=verify_signatures
!endif
CLEAN=verify_signatures

test: test-manifest-exec
    $(MAKE) "TARGET=test" $(TARGETS)

# We want `test` to be the default target
!include ..\Header.mak

test-manifest-exec:
    cd ..
    $(MAKE) test-manifest
    cd test

# validate certificates and binary metadata on executables
# TODO: move to buildtools?
verify_signatures: .virtual
    cd $(ROOT)\src\buildtools\verify_signatures
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

!include ..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
