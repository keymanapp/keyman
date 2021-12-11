#
# Keymanx64 Makefile
#

!include ..\..\Defines.mak

PLATFORM=x64

build: manifest.res version.res dirs
    $(MSBUILD) keymanx64.vcxproj $(MSBUILD_BUILD) "/p:Platform=x64"
    $(COPY) $(X64_TARGET_PATH)\keymanx64.exe $(PROGRAM)\engine
    $(COPY) $(X64_TARGET_PATH)\keymanx64.pdb $(DEBUGPATH)\engine

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

install:
    $(COPY) $(PROGRAM)\engine\keymanx64.exe "$(INSTALLPATH_KEYMANENGINE)"

test-uiaccess:
    findstr "uiAccess=\"true\"" manifest.in >nul

test-manifest:
# test that (a) linked manifest exists and correct, and (b) has uiAccess=true
    $(MT) -nologo -inputresource:$(PROGRAM)\engine\keymanx64.exe -validate_manifest
    $(MT) -nologo -inputresource:$(PROGRAM)\engine\keymanx64.exe -out:temp.manifest
    findstr "uiAccess=\"true\"" temp.manifest >nul
    -del temp.manifest

!include ..\..\Target.mak
