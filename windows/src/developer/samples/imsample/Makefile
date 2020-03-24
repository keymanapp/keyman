#
# imsample Makefile
#

VERSION_TXT_PATH=..\..

!include ..\..\..\Defines.mak

build: dirs version.res
    $(MSBUILD) imsample.sln $(MSBUILD_BUILD)
    $(COPY) imsample.dll $(PROGRAM)\developer

signcode:
    $(SIGNCODE) /d "Keyman IMX Sample" $(PROGRAM)\developer\imsample.dll

backup:
    $(WZZIP) $(BUILD)\developer\imsample.zip  $(BACKUPDEFAULTS) imsample.dll

clean: def-clean
    $(MSBUILD) imsample.sln $(MSBUILD_CLEAN)

install:
    @rem

!include ..\..\..\Target.mak
