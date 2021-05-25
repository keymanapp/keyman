#
# Unit Tests Makefile.
#

# ----------------------------------------------------------------------

TARGETS=group-helper-rsp19902 kmcomp shared-data lexical-model-compiler kmcomp-x64-structures \
        androidstringtokeymanlocalestring \
        keyboard-info keyboard-js-info keyboard-package-versions \
        kmx-file-languages package-info jsonutil windows-setup \
        ui-language-manager standards-data compile-supplementary-support

test:
    $(MAKE) -DTARGET=test $(TARGETS)

!include ..\..\Header.mak

# ----------------------------------------------------------------------

androidstringtokeymanlocalestring:
    cd $(ROOT)\src\test\unit-tests\androidstringtokeymanlocalestring
    $(MAKE) $(TARGET)

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

kmcomp-x64-structures:
	cd $(ROOT)\src\test\unit-tests\group-helper-rsp19902
	$(MAKE) $(TARGET)

keyboard-info:
    cd $(ROOT)\src\test\unit-tests\keyboard-info
    $(MAKE) $(TARGET)

keyboard-package-versions:
    cd $(ROOT)\src\test\unit-tests\keyboard-package-versions
    $(MAKE) $(TARGET)

keyboard-js-info:
    cd $(ROOT)\src\test\unit-tests\keyboard-js-info
    $(MAKE) $(TARGET)

kmx-file-languages:
    cd $(ROOT)\src\test\unit-tests\kmx-file-languages
    $(MAKE) $(TARGET)

package-info:
    cd $(ROOT)\src\test\unit-tests\package-info
    $(MAKE) $(TARGET)

jsonutil:
    cd $(ROOT)\src\test\unit-tests\jsonutil
    $(MAKE) $(TARGET)

windows-setup:
    cd $(ROOT)\src\test\unit-tests\windows-setup
    $(MAKE) $(TARGET)

ui-language-manager:
    cd $(ROOT)\src\test\unit-tests\ui-language-manager
    $(MAKE) $(TARGET)

standards-data:
    cd $(ROOT)\src\test\unit-tests\standards-data
    $(MAKE) $(TARGET)

compile-supplementary-support:
    cd $(ROOT)\src\test\unit-tests\compile-supplementary-support
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

!include ..\..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
