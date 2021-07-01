#
# Mkver Makefile
#

EXCLUDEPATHDEFINES=1

!include ..\..\Defines.mak

build: dirs
    # build with DCC32PATH as $DCC32 command uses devtools.exe...
    # $(DCC32) mkver.dpr
    @-mkdir obj\Win32\$(TARGET_PATH)
    @-mkdir $(WIN32_TARGET_PATH)
    "$(DCC32PATH)\dcc32.exe" -Q -B -E$(WIN32_TARGET_PATH) -NUobj\Win32\$(TARGET_PATH) -NSVcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Web;Soap;Winapi;System.Win  mkver.dpr
    $(COPY) $(WIN32_TARGET_PATH)\mkver.exe $(PROGRAM)\buildtools

clean: def-clean

!include ..\..\Target.mak
