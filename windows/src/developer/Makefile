#
# Keyman dev Makefile
#


TARGETS=kmcmpdll kmcomp kmanalyze kmconvert tike samples setup inst
MANIFESTS=kmcomp tike setup
CLEANS=clean-developer

!include ..\Header.mak

# ----------------------------------------------------------------------

imsample:
    cd $(ROOT)\src\developer\imsample
    $(MAKE) $(TARGET)

kmcmpdll:
    cd $(ROOT)\src\developer\kmcmpdll
    $(MAKE) $(TARGET)

kmcomp: kmcmpdll
    cd $(ROOT)\src\developer\kmcomp
    $(MAKE) $(TARGET)

kmanalyze:
    cd $(ROOT)\src\developer\kmanalyze
    $(MAKE) $(TARGET)

kmconvert:
    cd $(ROOT)\src\developer\kmconvert
    $(MAKE) $(TARGET)

tike: kmcmpdll
    cd $(ROOT)\src\developer\tike
    $(MAKE) $(TARGET)

samples:
    cd $(ROOT)\src\developer\samples
    $(MAKE) $(TARGET)

setup:
    cd $(ROOT)\src\developer\setup
    $(MAKE) $(TARGET)

inst:
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
