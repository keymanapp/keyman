#
# Unit Tests Makefile.
#

# ----------------------------------------------------------------------

TARGETS=windows-tests shared-tests

test:
    $(MAKE) "TARGET=test" $(TARGETS)

!include ..\..\Header.mak

# ----------------------------------------------------------------------

# TODO: move shared-tests to /common
shared-tests: androidstringtokeymanlocalestring jsonutil standards-data

windows-tests: group-helper-rsp19902 shared-data \
        windows-setup ui-language-manager

# ----------------------------------------------------------------------

androidstringtokeymanlocalestring: .virtual
    cd $(ROOT)\src\test\unit-tests\androidstringtokeymanlocalestring
    $(MAKE) $(TARGET)

group-helper-rsp19902: .virtual
    cd $(ROOT)\src\test\unit-tests\group-helper-rsp19902
    $(MAKE) $(TARGET)

shared-data: .virtual
    cd $(ROOT)\src\test\unit-tests\shared-data
    $(MAKE) $(TARGET)

jsonutil: .virtual
    cd $(ROOT)\src\test\unit-tests\jsonutil
    $(MAKE) $(TARGET)

windows-setup: .virtual
    cd $(ROOT)\src\test\unit-tests\windows-setup
    $(MAKE) $(TARGET)

ui-language-manager: .virtual
    cd $(ROOT)\src\test\unit-tests\ui-language-manager
    $(MAKE) $(TARGET)

standards-data: .virtual
    cd $(ROOT)\src\test\unit-tests\standards-data
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

!include ..\..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
