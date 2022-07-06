#
# jedi Makefile (Keyman project)
#
# We have made our own copy of package files from the JEDI project
# in the Packages folder. If we upgrade to a new version of Delphi,
# we may need to update these package files. This was the easiest
# way to support Delphi Starter, which does not have commmand line
# build, and so we could control the output path of the packages.
#

!include ..\..\Defines.mak

build: dirs
    $(MAKE) install-paths
    $(MAKE) build-jvcl-docking
    $(MAKE) install-packages

build-jvcl-docking:
    cd $(DEVELOPER_ROOT)\src\ext\jedi\packages
    @-mkdir obj\Win32\$(TARGET_PATH)
    $(DCC32DPK) Jcl.dpk
    $(DCC32DPK) JclVcl.dpk
    $(DCC32DPK) JvCore.dpk
    $(DCC32DPK) JvCoreDesign.dpk
    $(DCC32DPK) JvDocking.dpk
    $(DCC32DPK) JvDockingDesign.dpk

install: install-paths install-packages

install-paths:
    $(DEVTOOLS) -ai $(DEVELOPER_ROOT)\src\ext\jedi

    $(DEVTOOLS) -ai $(DEVELOPER_ROOT)\src\ext\jedi\jcl\jcl\source\common
    $(DEVTOOLS) -ai $(DEVELOPER_ROOT)\src\ext\jedi\jcl\jcl\source\include
    $(DEVTOOLS) -ai $(DEVELOPER_ROOT)\src\ext\jedi\jcl\jcl\source\vcl
    $(DEVTOOLS) -ai $(DEVELOPER_ROOT)\src\ext\jedi\jcl\jcl\source\windows

    $(DEVTOOLS) -ai $(DEVELOPER_ROOT)\src\ext\jedi\jvcl\jvcl\common
    $(DEVTOOLS) -ai $(DEVELOPER_ROOT)\src\ext\jedi\jvcl\jvcl\resources
    $(DEVTOOLS) -ai $(DEVELOPER_ROOT)\src\ext\jedi\jvcl\jvcl\run

    $(DEVTOOLS) -ai $(DEVELOPER_ROOT)\src\ext\jedi\packages

install-packages:
    $(DEVTOOLS) -ip $(OUTLIB)\JvCoreDesign.bpl
    $(DEVTOOLS) -ip $(OUTLIB)\JvDockingDesign.bpl

clean:
    rem

!include ..\..\Target.mak
