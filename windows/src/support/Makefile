#
# Support Makefile
#

!ifdef NODELPHI
TARGETS=etl2log
!else
TARGETS=oskbulkrenderer etl2log
!endif
CLEANS=clean-support

!include ..\Header.mak

# ----------------------------------------------------------------------

oskbulkrenderer: .virtual
    cd $(ROOT)\src\support\oskbulkrenderer
    $(MAKE) $(TARGET)

etl2log: .virtual
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
