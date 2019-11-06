#
# Setup Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res icons
    $(DCC32) setup.dpr
    $(TDS2DBG) setup.exe
    $(MAKEJCLDBG) setup.map
    $(COPY) setup.exe $(PROGRAM)\desktop

setup_dialog.res:
    rc setup_dialog.rc

icons:
    rc icons.rc

clean: def-clean
    rem if exist icons\icons.res del icons\icons.res

signcode:
    rem SIGNCODE /d "Keyman Desktop Setup" $(PROGRAM)\desktop\setup.exe

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
