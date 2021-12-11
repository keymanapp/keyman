#
# Keyman for Windows Makefile
#

TARGETS=kmshell kmconfig kmbrowserhost setup insthelp
RELEASE_TARGETS=help
CLEANS=inst clean-desktop
MANIFESTS=kmshell setup insthelp

!include ..\Header.mak

# ----------------------------------------------------------------------

kmshell: .virtual
    cd $(ROOT)\src\desktop\kmshell
    $(MAKE) $(TARGET)

kmconfig: .virtual
    cd $(ROOT)\src\desktop\kmconfig
    $(MAKE) $(TARGET)

kmbrowserhost: .virtual
    cd $(ROOT)\src\desktop\kmbrowserhost
    $(MAKE) $(TARGET)

setup: .virtual
    cd $(ROOT)\src\desktop\setup
    $(MAKE) $(TARGET)

help: .virtual
    cd $(ROOT)\src\desktop\help
    $(MAKE) $(TARGET)

insthelp: .virtual
  cd $(ROOT)\src\desktop\insthelp
  $(MAKE) $(TARGET)

inst: .virtual
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
