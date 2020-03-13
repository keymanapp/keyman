#
# InstHelper Makefile
#

VERSION_TXT_PATH=..\..

!include ..\..\..\Defines.mak

build: dirs version.res
    $(DCC32) insthelper.dpr
    $(TDS2DBG) insthelper.dll
    $(SENTRYTOOL_DELPHIPREP) insthelper.dll -dpr insthelper.dpr
    $(COPY) insthelper.dll $(PROGRAM)\inst

clean: def-clean
    -del *.dll

signcode:
    $(SIGNCODE) /d "Keyman Engine Install Helper" $(PROGRAM)\inst\insthelper.dll

backup:
    $(WZZIP) $(BUILD)\inst\insthelper.zip  $(BACKUPDEFAULTS) insthelper.dll

!include ..\..\..\Target.mak
