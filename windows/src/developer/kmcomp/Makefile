#
# Kmcomp Makefile
#

!include ..\..\Defines.mak

TARGET_BASE=kmcomp
TARGET_EXT=exe
TARGET_GROUP=developer

build: version.res manifest.res dirs icons
    $(DELPHI_MSBUILD) kmcomp.dproj /p:Platform=Win32

    $(SENTRYTOOL_DELPHIPREP) $(WIN32_TARGET_PATH)\kmcomp.exe -dpr kmcomp.dpr
    $(TDS2DBG) $(WIN32_TARGET_PATH)\kmcomp.exe
    $(COPY) $(WIN32_TARGET_PATH)\kmcomp.exe $(PROGRAM)\developer
    if exist $(WIN32_TARGET_PATH)\kmcomp.dbg $(COPY) $(WIN32_TARGET_PATH)\kmcomp.dbg $(DEBUGPATH)\developer

    $(DELPHI_MSBUILD) kmcomp.dproj /p:Platform=Win64

    if exist $(WIN64_TARGET_PATH)\kmcomp.x64.exe del $(WIN64_TARGET_PATH)\kmcomp.x64.exe
    # Delphi does not allow us to build to a different target filename so we rename after build
    ren $(WIN64_TARGET_PATH)\kmcomp.exe kmcomp.x64.exe

    if exist $(WIN64_TARGET_PATH)\kmcomp.x64.map del $(WIN64_TARGET_PATH)\kmcomp.x64.map
    ren $(WIN64_TARGET_PATH)\kmcomp.map kmcomp.x64.map

    # $(SENTRYTOOL_DELPHIPREP) $(WIN64_TARGET_PATH)\kmcomp.x64.exe -dpr kmcomp.dpr
    $(COPY) $(WIN64_TARGET_PATH)\kmcomp.x64.exe $(PROGRAM)\developer\kmcomp.x64.exe
    if exist $(WIN64_TARGET_PATH)\kmcomp.dbg $(COPY) $(WIN64_TARGET_PATH)\kmcomp.dbg $(DEBUGPATH)\developer\kmcomp.x64.dbg

icons:
    rc icons.rc

clean: def-clean
    if exist icons.res del icons.res

signcode:
    $(SIGNCODE) /d "Keyman Developer Command-Line Compiler" $(PROGRAM)\developer\kmcomp.exe
    $(SIGNCODE) /d "Keyman Developer Command-Line Compiler" $(PROGRAM)\developer\kmcomp.x64.exe

backup:
    $(WZZIP) $(BUILD)\developer\kmcomp.zip  $(BACKUPDEFAULTS) $(PROGRAM)\developer\kmcomp.exe $(PROGRAM)\developer\kmcomp.x64.exe

test-manifest:
    # test that (a) linked manifest exists and correct
    $(MT) -nologo -inputresource:$(PROGRAM)\developer\kmcomp.exe -validate_manifest
    $(MT) -nologo -inputresource:$(PROGRAM)\developer\kmcomp.x64.exe -validate_manifest

install:
    $(COPY) $(PROGRAM)\developer\kmcomp.exe "$(INSTALLPATH_KEYMANDEVELOPER)\kmcomp.exe"
    $(COPY) $(PROGRAM)\developer\kmcomp.exe "$(INSTALLPATH_KEYMANDEVELOPER)\kmcomp.x64.exe"

!include ..\..\Target.mak
