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
    #$(MAKE) build-jcldbg
    $(MAKE) install-packages

build-jcldbg:
  rem
    # This needs to be called separately so we get the updated include path jedi-includes
    # It also needs to be called after make install
#    cd $(EXT)\jedi\jcl\jcl\examples\windows\debug\tools

    # We use a direct build to ignore platform warnings
    # as we don't control the source of this code

#    $(MAKEDIR)\dcc32.exe $(DELPHIDPRPARAMS) makejcldbg.dpr
#    $(COPY) makejcldbg.exe $(PROGRAM)\buildtools

build-jvcl-docking:
    cd $(EXT)\jedi\packages
#    $(DCC32DPK) Jcl.dpk
#    $(DCC32DPK) JclVcl.dpk
    $(DCC32DPK) JvCore.dpk
    $(DCC32DPK) JvCoreDesign.dpk
    $(DCC32DPK) JvDocking.dpk
    $(DCC32DPK) JvDockingDesign.dpk

install: install-paths install-packages

install-paths:
    $(DEVTOOLS) -ai $(EXT)\jedi

#    $(DEVTOOLS) -ai $(EXT)\jedi\jcl\jcl\source\common
#    $(DEVTOOLS) -ai $(EXT)\jedi\jcl\jcl\source\include
#    $(DEVTOOLS) -ai $(EXT)\jedi\jcl\jcl\source\vcl
#    $(DEVTOOLS) -ai $(EXT)\jedi\jcl\jcl\source\windows

    $(DEVTOOLS) -ai $(EXT)\jedi\jvcl\jvcl\common
    $(DEVTOOLS) -ai $(EXT)\jedi\jvcl\jvcl\resources
    $(DEVTOOLS) -ai $(EXT)\jedi\jvcl\jvcl\run

    $(DEVTOOLS) -ai $(EXT)\jedi\jwa\branches\2.3\Win32API

    $(DEVTOOLS) -ai $(EXT)\jedi\packages

install-packages:
    $(DEVTOOLS) -ip $(OUTLIB)\JvCoreDesign.bpl
    $(DEVTOOLS) -ip $(OUTLIB)\JvDockingDesign.bpl

clean:
    cd $(EXT)\jedi\jcl\jcl\examples\windows\debug\tools
    -del *.exe

!include ..\..\Target.mak
