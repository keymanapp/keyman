#
# Support Makefile
#

TARGETS=oskbulkrenderer etl2log
CLEANS=clean-support

!include ..\Header.mak

# ----------------------------------------------------------------------

oskbulkrenderer:
    cd $(ROOT)\src\support\oskbulkrenderer
    $(MAKE) $(TARGET)

etl2log:
    cd $(ROOT)\src\support\etl2log
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

clean-support:
    cd $(ROOT)\src\support
    -del version.txt

!include ..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
