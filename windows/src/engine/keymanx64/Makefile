#
# Keymanx64 Makefile
#

!include ..\..\Defines.mak

PLATFORM=x64

build: manifest.res version.res dirs
    $(MSBUILD) keymanx64.sln $(MSBUILD_BUILD)
    $(COPY) keymanx64.exe $(PROGRAM)\engine
    $(COPY) keymanx64.pdb $(DEBUGPATH)\engine

clean: def-clean
    $(MSBUILD) $(MSBUILD_CLEAN) keymanx64.sln
    -del x64\release\buildlog.htm
    -rmdir x64\release
    -del x64\debug\buildlog.htm
    -rmdir x64\debug
    -rmdir x64

signcode:
    $(SIGNCODE) /d "Keyman Engine x64" $(PROGRAM)\engine\keymanx64.exe

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\engine\keymanx64.exe /t keyman-engine-windows
    $(SYMSTORE) $(DEBUGPATH)\engine\keymanx64.pdb /t keyman-engine-windows

backup:
    $(WZZIP) $(BUILD)\engine\keymanx64.exe  $(BACKUPDEFAULTS) keymanx64.exe

install:
    $(COPY) $(PROGRAM)\engine\keymanx64.exe "$(INSTALLPATH_KEYMANENGINE)"

test-uiaccess:
    grep -c "uiAccess=\"true\"" manifest.in

test-manifest:
    # test that (a) linked manifest exists and correct, and (b) has uiAccess=true
    $(MT) -nologo -inputresource:$(PROGRAM)\engine\keymanx64.exe -validate_manifest
    $(MT) -nologo -inputresource:$(PROGRAM)\engine\keymanx64.exe -out:temp.manifest
    grep -c "uiAccess=\"true\"" temp.manifest
    -del temp.manifest

!include ..\..\Target.mak
