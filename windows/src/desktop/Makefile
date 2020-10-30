#
# Keyman Desktop Makefile
#

TARGETS=kmshell kmconfig kmbrowserhost setup insthelp
RELEASE_TARGETS=help
CLEANS=inst clean-desktop
MANIFESTS=kmshell setup insthelp

!include ..\Header.mak

# ----------------------------------------------------------------------

kmshell:
    cd $(ROOT)\src\desktop\kmshell
    $(MAKE) $(TARGET)

kmconfig:
    cd $(ROOT)\src\desktop\kmconfig
    $(MAKE) $(TARGET)

kmbrowserhost:
    cd $(ROOT)\src\desktop\kmbrowserhost
    $(MAKE) $(TARGET)

setup:
    cd $(ROOT)\src\desktop\setup
    $(MAKE) $(TARGET)

help:
    cd $(ROOT)\src\desktop\help
    $(MAKE) $(TARGET)

insthelp:
  cd $(ROOT)\src\desktop\insthelp
  $(MAKE) $(TARGET)

inst:
  cd $(ROOT)\src\desktop\inst
  $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

clean-desktop:
    cd $(ROOT)\src\desktop
    -del version.txt

!include ..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
