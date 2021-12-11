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
    cd $(ROOT)\src\developer\imsample
    $(MAKE) $(TARGET)

kmcmpdll: .virtual
    cd $(ROOT)\src\developer\kmcmpdll
    $(MAKE) $(TARGET)

kmdecomp: .virtual
    cd $(ROOT)\src\developer\kmdecomp
    $(MAKE) $(TARGET)

kmcomp: kmcmpdll
    cd $(ROOT)\src\developer\kmcomp
    $(MAKE) $(TARGET)

kmanalyze: .virtual
    cd $(ROOT)\src\developer\kmanalyze
    $(MAKE) $(TARGET)

kmconvert: .virtual
    cd $(ROOT)\src\developer\kmconvert
    $(MAKE) $(TARGET)

tike: kmcmpdll redist
    cd $(ROOT)\src\developer\tike
    $(MAKE) $(TARGET)

samples: .virtual
    cd $(ROOT)\src\developer\samples
    $(MAKE) $(TARGET)

setup: .virtual
    cd $(ROOT)\src\developer\setup
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

redist:
    copy $(ROOT)\src\ext\sentry\sentry.dll $(ROOT)\bin\developer
    copy $(ROOT)\src\ext\sentry\sentry.x64.dll $(ROOT)\bin\developer
    copy $(ROOT)\src\ext\sentry\crashpad_handler.exe $(ROOT)\bin\developer

# ----------------------------------------------------------------------

inst: redist
    cd $(ROOT)\src\developer\inst
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

clean-developer:
    cd $(ROOT)\src\developer
    -del version.txt

!include ..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
