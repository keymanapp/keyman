#
# Unit Tests Makefile.
#

# ----------------------------------------------------------------------

TARGETS=keyboard-info keyboard-js-info keyboard-package-versions \
        kmx-file-languages package-info new-tests jsonutil windows-setup \
        install-default-ui-language

test:
    $(MAKE) -DTARGET=test $(TARGETS)

!include ..\Header.mak

# ----------------------------------------------------------------------

keyboard-info:
    cd $(ROOT)\src\unit-tests\keyboard-info
    $(MAKE) $(TARGET)

keyboard-package-versions:
    cd $(ROOT)\src\unit-tests\keyboard-package-versions
    $(MAKE) $(TARGET)

keyboard-js-info:
    cd $(ROOT)\src\unit-tests\keyboard-js-info
    $(MAKE) $(TARGET)

kmx-file-languages:
    cd $(ROOT)\src\unit-tests\kmx-file-languages
    $(MAKE) $(TARGET)

package-info:
    cd $(ROOT)\src\unit-tests\package-info
    $(MAKE) $(TARGET)

jsonutil:
    cd $(ROOT)\src\unit-tests\jsonutil
    $(MAKE) $(TARGET)

windows-setup:
    cd $(ROOT)\src\unit-tests\windows-setup
    $(MAKE) $(TARGET)

install-default-ui-language:
    cd $(ROOT)\src\unit-tests\install-default-ui-language
    $(MAKE) $(TARGET)

new-tests:
    cd $(ROOT)\src\test\unit-tests
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

!include ..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
