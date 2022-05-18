#
# Keyman dev Makefile
#

!ifdef NODELPHI
TARGETS=kmcmpdll kmanalyze kmdecomp
!else
TARGETS=kmcmpdll kmcomp kmanalyze kmconvert tike samples setup inst kmdecomp
!endif

MANIFESTS=kmcomp tike setup
CLEANS=clean-developer

!include ..\Header.mak

# ----------------------------------------------------------------------

imsample: .virtual
    cd $(DEVELOPER_ROOT)\src\imsample
    $(MAKE) $(TARGET)

kmcmpdll: .virtual
    cd $(DEVELOPER_ROOT)\src\kmcmpdll
    $(MAKE) $(TARGET)

kmdecomp: .virtual
    cd $(DEVELOPER_ROOT)\src\kmdecomp
    $(MAKE) $(TARGET)

kmcomp: kmcmpdll
    cd $(DEVELOPER_ROOT)\src\kmcomp
    $(MAKE) $(TARGET)

kmanalyze: .virtual
    cd $(DEVELOPER_ROOT)\src\kmanalyze
    $(MAKE) $(TARGET)

kmconvert: .virtual
    cd $(DEVELOPER_ROOT)\src\kmconvert
    $(MAKE) $(TARGET)

tike: kmcmpdll redist
    cd $(DEVELOPER_ROOT)\src\tike
    $(MAKE) $(TARGET)

samples: .virtual
    cd $(DEVELOPER_ROOT)\src\samples
    $(MAKE) $(TARGET)

setup: .virtual
    cd $(DEVELOPER_ROOT)\src\setup
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

redist:
    copy $(WINDOWS_ROOT)\src\ext\sentry\sentry.dll $(DEVELOPER_ROOT)\bin
    copy $(WINDOWS_ROOT)\src\ext\sentry\sentry.x64.dll $(DEVELOPER_ROOT)\bin
    copy $(WINDOWS_ROOT)\src\ext\sentry\crashpad_handler.exe $(DEVELOPER_ROOT)\bin

# ----------------------------------------------------------------------

inst: redist
    cd $(DEVELOPER_ROOT)\src\inst
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

clean-developer:
    cd $(DEVELOPER_ROOT)\src
    -del version.txt

!include ..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
