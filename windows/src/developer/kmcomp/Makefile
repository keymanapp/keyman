#
# Kmcomp Makefile
#

!include ..\..\Defines.mak

TARGET_BASE=kmcomp
TARGET_EXT=exe
TARGET_GROUP=developer

build: version.res manifest.res dirs icons
    $(DELPHI_MSBUILD) $(TARGET_BASE).dproj /p:Platform=Win32

    $(TDS2DBG) $(WIN32_TARGET_PATH)\$(TARGET_BASE).$(TARGET_EXT)
    $(SENTRYTOOL_DELPHIPREP) $(WIN32_TARGET_PATH)\$(TARGET_BASE).$(TARGET_EXT)
    $(COPY) $(WIN32_TARGET_PATH)\$(TARGET_BASE).$(TARGET_EXT) $(PROGRAM)\$(TARGET_GROUP)
    if exist $(WIN32_TARGET_PATH)\$(TARGET_BASE).dbg $(COPY) $(WIN32_TARGET_PATH)\$(TARGET_BASE).dbg $(DEBUGPATH)\$(TARGET_GROUP)

    $(DELPHI_MSBUILD) $(TARGET_BASE).dproj /p:Platform=Win64

    if exist $(WIN64_TARGET_PATH)\$(TARGET_BASE).x64.$(TARGET_EXT) del $(WIN64_TARGET_PATH)\$(TARGET_BASE).x64.$(TARGET_EXT)
    $(MAKEJCLDBG) $(WIN64_TARGET_PATH)\$(TARGET_BASE).map
    # Delphi does not allow us to build to a different target filename so we rename after build
    ren $(WIN64_TARGET_PATH)\$(TARGET_BASE).$(TARGET_EXT) $(TARGET_BASE).x64.$(TARGET_EXT)
    $(COPY) $(WIN64_TARGET_PATH)\$(TARGET_BASE).x64.$(TARGET_EXT) $(PROGRAM)\$(TARGET_GROUP)\$(TARGET_BASE).x64.$(TARGET_EXT)
    if exist $(WIN64_TARGET_PATH)\$(TARGET_BASE).dbg $(COPY) $(WIN64_TARGET_PATH)\$(TARGET_BASE).dbg $(DEBUGPATH)\$(TARGET_GROUP)\$(TARGET_BASE).x64.dbg

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
