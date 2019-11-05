#
# Mkver Makefile
#

EXCLUDEPATHDEFINES=1

!include ..\..\Defines.mak

build: dirs
    # build with DCC32PATH as $DCC32 command uses devtools.exe...
    # $(DCC32) mkver.dpr
    "$(DCC32PATH)\dcc32.exe" -Q -B -E. -NSVcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Web;Soap;Winapi;System.Win mkver.dpr
    $(COPY) mkver.exe $(PROGRAM)\buildtools

clean: def-clean

!include ..\..\Target.mak
