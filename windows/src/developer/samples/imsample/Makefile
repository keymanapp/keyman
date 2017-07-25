#
# imsample Makefile
#

!include ..\..\..\Defines.mak

build: dirs
    # version.res target assumes version.txt in parent folder so use this line below instead
    $(ROOT)\bin\buildtools\mkver -v ..\..\version.txt
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
