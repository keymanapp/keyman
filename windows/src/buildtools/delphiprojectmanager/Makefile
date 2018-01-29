#
# Devtools Makefile
#

!include ..\..\Defines.mak

build: dirs
    # build with DCC32 as $DCC32 command uses devtools.exe, and devtools uses this package...
!ifdef DELPHI_STARTER
    # Don't build this automatically for Delphi Starter. The IDE sometimes thinks a file has
    # changed and then throws up a save confirmation dialog.
!else
    $(MAKEDIR)\dcc32 -B -E. -I$(DELPHIINCLUDES) -U$(DELPHIINCLUDES) -NSVcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Web;Soap;Winapi;System.Win -LE$(OUTLIB) -LN$(OUTLIB) delphiprojectmanager.dpk
    $(DEVTOOLS) -ip $(OUTLIB)\delphiprojectmanager.bpl
!endif

clean: def-clean

!include ..\..\Target.mak
