#
# Setup Makefile
#

!include ..\Defines.mak

build: version.res manifest.res icons
    $(DELPHI_MSBUILD) setup.dproj "/p:Platform=Win32"
    $(SENTRYTOOL_DELPHIPREP) $(WIN32_TARGET_PATH)\setup.exe -dpr setup.dpr
    $(TDS2DBG) $(WIN32_TARGET_PATH)\setup.exe
    $(COPY) $(WIN32_TARGET_PATH)\setup.exe $(DEVELOPER_PROGRAM)
    if exist $(WIN32_TARGET_PATH)\setup.dbg $(COPY) $(WIN32_TARGET_PATH)\setup.dbg $(DEVELOPER_DEBUGPATH)\devsetup.dbg
    -del $(WIN32_TARGET_PATH)\devsetup.dbg
    if exist $(WIN32_TARGET_PATH)\setup.dbg ren $(WIN32_TARGET_PATH)\setup.dbg devsetup.dbg

setup_dialog.res:
    rc setup_dialog.rc

icons:
    rc icons.rc

clean: def-clean
    rem if exist icons\icons.res del icons\icons.res

signcode:
    rem SIGNCODE /d "Keyman Developer Setup" $(DEVELOPER_PROGRAM)\setup.exe

wrap-symbols:
# This is slightly non-standard due to renamed devsetup.dbg above
# we should be able to clean this up in the future when we remove the
# zipped debug symbols
    $(SYMSTORE) $(DEVELOPER_PROGRAM)\setup.exe /t keyman-developer
    $(SYMSTORE) $(DEVELOPER_ROOT)\src\setup\setup.dbg /t keyman-developer

test-manifest:
# test that (a) linked manifest exists and correct, and (b) has uiAccess=true
    $(MT) -nologo -inputresource:$(DEVELOPER_PROGRAM)\setup.exe -validate_manifest

install:
    @rem

!include ..\Target.mak
