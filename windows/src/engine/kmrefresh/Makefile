#
# kmrefresh Makefile
#

!include ..\..\Defines.mak

PLATFORM=x64

build: version.res dirs
    $(MSBUILD) kmrefresh.vcxproj /p:Platform=Win32 $(MSBUILD_BUILD)
    $(MSBUILD) kmrefresh.vcxproj /p:Platform=x64 $(MSBUILD_BUILD)
    $(COPY) $(WIN32_TARGET_PATH)\kmrefresh.x86.exe $(PROGRAM)\engine
    $(COPY) $(WIN32_TARGET_PATH)\kmrefresh.x86.pdb $(DEBUGPATH)\engine
    $(COPY) $(X64_TARGET_PATH)\kmrefresh.x64.exe $(PROGRAM)\engine
    $(COPY) $(X64_TARGET_PATH)\kmrefresh.x64.pdb $(DEBUGPATH)\engine

clean: def-clean
    $(MSBUILD) $(MSBUILD_CLEAN) kmrefresh.vcxproj
    -rd /s/q x64
    -rd /s/q Win32

signcode:
    $(SIGNCODE) /d "Keyman Engine Refresh x86" $(PROGRAM)\engine\kmrefresh.x86.exe
    $(SIGNCODE) /d "Keyman Engine Refresh x64" $(PROGRAM)\engine\kmrefresh.x64.exe

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\engine\kmrefresh.x86.exe /t keyman-engine-windows
    $(SYMSTORE) $(DEBUGPATH)\engine\kmrefresh.x86.pdb /t keyman-engine-windows
    $(SYMSTORE) $(PROGRAM)\engine\kmrefresh.x64.exe /t keyman-engine-windows
    $(SYMSTORE) $(DEBUGPATH)\engine\kmrefresh.x64.pdb /t keyman-engine-windows

backup:
    $(WZZIP) $(BUILD)\engine\kmrefresh.x86.exe  $(BACKUPDEFAULTS) kmrefresh.x86.exe
    $(WZZIP) $(BUILD)\engine\kmrefresh.x64.exe  $(BACKUPDEFAULTS) kmrefresh.x64.exe

install:
    $(COPY) $(PROGRAM)\engine\kmrefresh.x86.exe "$(INSTALLPATH_KEYMANENGINE)"
    $(COPY) $(PROGRAM)\engine\kmrefresh.x64.exe "$(INSTALLPATH_KEYMANENGINE)"

!include ..\..\Target.mak
