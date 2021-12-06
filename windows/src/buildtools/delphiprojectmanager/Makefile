#
# Devtools Makefile
#

!include ..\..\Defines.mak

build: dirs
# build with DCC32 as $DCC32 command uses devtools.exe, and devtools uses this package...
    @-mkdir obj\Win32\$(TARGET_PATH)
    cmd /c "$(DCC32PATH)\dcc32.exe" -B -E. -I$(DELPHIINCLUDES) -U$(DELPHIINCLUDES) -NSVcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Web;Soap;Winapi;System.Win -LE$(OUTLIB) -LN$(OUTLIB) -NUobj\Win32\$(TARGET_PATH) delphiprojectmanager.dpk
    $(DEVTOOLS) -ip $(OUTLIB)\delphiprojectmanager.bpl

clean: def-clean

!include ..\..\Target.mak
