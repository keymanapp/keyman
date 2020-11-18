#
# Unit Tests Makefile.
#

# ----------------------------------------------------------------------

TARGETS=group-helper-rsp19902 kmcomp shared-data lexical-model-compiler kmcomp-x64-structures

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

# Revert any local changes to the original source; these changes can break
# later sub-builds.
    cd $(KEYMAN_ROOT)\developer\js
		git restore package.json
		git restore package-lock.json

kmcomp-x64-structures:
	cd $(ROOT)\src\test\unit-tests\group-helper-rsp19902
	$(MAKE) $(TARGET)

# ----------------------------------------------------------------------

!include ..\..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
