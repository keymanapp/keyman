#
# Setup Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res icons locale
    $(DELPHI_MSBUILD) setup.dproj "/p:Platform=Win32"
    $(SENTRYTOOL_DELPHIPREP) $(WIN32_TARGET_PATH)\setup.exe -dpr setup.dpr
    $(TDS2DBG) $(WIN32_TARGET_PATH)\setup.exe
    $(COPY) $(WIN32_TARGET_PATH)\setup.exe $(PROGRAM)\desktop

# This version of setup.exe is for redistribution as a
# bundled installer and is not signed
    $(COPY) $(WIN32_TARGET_PATH)\setup.exe $(PROGRAM)\desktop\setup-redist.exe

locale: .virtual
    $(DEVTOOLS) -buildsetupstrings locale\ .\

setup_dialog.res:
    rc setup_dialog.rc

icons:
    rc icons.rc

clean: def-clean
    rem if exist icons\icons.res del icons\icons.res

signcode:
    $(SIGNCODE) /d "Keyman Setup" $(PROGRAM)\desktop\setup.exe

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\desktop\setup.exe /t keyman-windows
    $(SYMSTORE) $(DEBUGPATH)\desktop\setup.dbg /t keyman-windows

test-manifest:
# test that (a) linked manifest exists and correct, and (b) has uiAccess=true
    $(MT) -nologo -inputresource:$(PROGRAM)\desktop\setup.exe -validate_manifest

debug-manifest:
# make a non-elevated manifest for testing purposes
    -del manifest.res
    copy manifest.in std-manifest.tmp
    copy debug-manifest.in manifest.in
    $(MAKE) manifest.res
    copy std-manifest.tmp manifest.in
    -del std-manifest.tmp

install:
    rem

!include ..\..\Target.mak
