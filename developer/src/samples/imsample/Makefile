#
# imsample Makefile
#

VERSION_TXT_PATH=..\..

!include ..\..\..\Defines.mak

build: dirs version.res
    $(MSBUILD) imsample.sln $(MSBUILD_BUILD) "/p:Platform=Win32"
    $(COPY) $(WIN32_TARGET_PATH)\imsample.dll $(PROGRAM)\developer
    $(COPY) $(WIN32_TARGET_PATH)\imsample.pdb $(PROGRAM)\developer

		$(MSBUILD) imsample.sln $(MSBUILD_BUILD) "/p:Platform=x64"
    $(COPY) $(X64_TARGET_PATH)\imsample.x64.dll $(PROGRAM)\developer
    $(COPY) $(X64_TARGET_PATH)\imsample.x64.pdb $(PROGRAM)\developer


signcode:
    $(SIGNCODE) /d "Keyman IMX Sample" $(PROGRAM)\developer\imsample.dll
		$(SIGNCODE) /d "Keyman IMX Sample" $(PROGRAM)\developer\imsample.x64.dll


wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\developer\imsample.dll /t keyman-developer
		$(SYMSTORE) $(PROGRAM)\developer\imsample.x64.dll /t keyman-developer
    $(SYMSTORE) $(DEBUGPATH)\developer\imsample.pdb /t keyman-developer
		$(SYMSTORE) $(DEBUGPATH)\developer\imsample.x64.pdb /t keyman-developer

clean: def-clean
    $(MSBUILD) imsample.sln $(MSBUILD_CLEAN)

install:
    @rem

!include ..\..\..\Target.mak
