#
# KMTIP Makefile
#

!include ..\..\Defines.mak

build: version.res dirs
    $(MSBUILD) kmtip.vcxproj $(MSBUILD_BUILD) /p:Platform=Win32
    $(COPY) $(WIN32_TARGET_PATH)\kmtip.dll $(PROGRAM)\engine
    $(COPY) $(WIN32_TARGET_PATH)\kmtip.pdb $(DEBUGPATH)\engine
    $(MSBUILD) kmtip.vcxproj $(MSBUILD_BUILD) /p:Platform=x64
    $(COPY) $(X64_TARGET_PATH)\kmtip64.dll $(PROGRAM)\engine\kmtip64.dll
    $(COPY) $(X64_TARGET_PATH)\kmtip64.pdb $(DEBUGPATH)\engine\kmtip64.pdb

clean: def-clean
    $(MSBUILD) kmtip.vcxproj $(MSBUILD_CLEAN) /p:Platform=Win32
    $(MSBUILD) kmtip.vcxproj $(MSBUILD_CLEAN) /p:Platform=x64

signcode:
    $(SIGNCODE) /d "Keyman Engine Text Service" $(PROGRAM)\engine\kmtip.dll
    $(SIGNCODE) /d "Keyman Engine Text Service" $(PROGRAM)\engine\kmtip64.dll

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\engine\kmtip.dll /t keyman-engine-windows
    $(SYMSTORE) $(PROGRAM)\engine\kmtip64.dll /t keyman-engine-windows
    $(SYMSTORE) $(DEBUGPATH)\engine\kmtip.pdb /t keyman-engine-windows
    $(SYMSTORE) $(DEBUGPATH)\engine\kmtip64.pdb /t keyman-engine-windows

install:
    $(COPY) $(PROGRAM)\engine\kmtip.dll "$(INSTALLPATH_KEYMANENGINE)\kmtip.dll"
    $(COPY) $(PROGRAM)\engine\kmtip64.dll "$(INSTALLPATH_KEYMANENGINE)\kmtip64.dll"

!include ..\..\Target.mak
