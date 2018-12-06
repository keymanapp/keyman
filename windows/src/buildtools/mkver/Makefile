#
# Mkver Makefile
#

EXCLUDEPATHDEFINES=1

!include ..\..\Defines.mak

build: dirs
    # build with DCC32 as $DCC32 command uses devtools.exe...
    # $(DCC32) mkver.dpr
    $(MAKEDIR)\dcc32 -Q -B -E. -NSVcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Web;Soap;Winapi;System.Win mkver.dpr
    $(COPY) mkver.exe $(PROGRAM)\buildtools

clean: def-clean

!include ..\..\Target.mak
