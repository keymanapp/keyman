#
# Setup Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res icons locale
    $(DCC32) setup.dpr
    $(SENTRYTOOL_DELPHIPREP) setup.exe -dpr setup.dpr
    $(TDS2DBG) setup.exe
    $(COPY) setup.exe $(PROGRAM)\desktop

locale:
    $(ROOT)\bin\buildtools\devtools.exe -buildsetupstrings locale\ .\

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

backup:
    $(WZZIP) $(BUILD)\desktop\setup.zip  $(BACKUPDEFAULTS) setup.exe

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
