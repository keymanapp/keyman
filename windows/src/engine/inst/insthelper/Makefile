#
# InstHelper Makefile
#

VERSION_TXT_PATH=..\..

!include ..\..\..\Defines.mak

build: dirs version.res
    $(DELPHI_MSBUILD) insthelper.dproj "/p:Platform=Win32"
    $(SENTRYTOOL_DELPHIPREP) $(WIN32_TARGET_PATH)\insthelper.dll -dpr insthelper.dpr
    $(TDS2DBG) $(WIN32_TARGET_PATH)\insthelper.dll
    $(COPY) $(WIN32_TARGET_PATH)\insthelper.dll $(PROGRAM)\inst
    $(COPY) $(WIN32_TARGET_PATH)\insthelper.dbg $(DEBUGPATH)\inst

clean: def-clean
    -del *.dll

signcode:
    $(SIGNCODE) /d "Keyman Engine Install Helper" $(PROGRAM)\inst\insthelper.dll

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\inst\insthelper.dll /t keyman-windows
    $(SYMSTORE) $(DEBUGPATH)\inst\insthelper.dbg /t keyman-windows

!include ..\..\..\Target.mak
