#
# Setup Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res icons
    $(DCC32) setup.dpr
    $(SENTRYTOOL_DELPHIPREP) setup.exe -dpr setup.dpr
    $(TDS2DBG) setup.exe
    $(COPY) setup.exe $(PROGRAM)\developer
    if exist setup.dbg $(COPY) setup.dbg $(DEBUGPATH)\developer\devsetup.dbg
    -del devsetup.dbg
    if exist setup.dbg ren setup.dbg devsetup.dbg

setup_dialog.res:
    rc setup_dialog.rc

icons:
    rc icons.rc

clean: def-clean
    rem if exist icons\icons.res del icons\icons.res

signcode:
    rem SIGNCODE /d "Keyman Developer Setup" $(PROGRAM)\developer\setup.exe

wrap-symbols:
    # This is slightly non-standard due to renamed devsetup.dbg above
    # we should be able to clean this up in the future when we remove the
    # zipped debug symbols
    $(SYMSTORE) $(PROGRAM)\developer\setup.exe /t keyman-developer
    $(SYMSTORE) $(ROOT)\src\developer\setup\setup.dbg /t keyman-developer

backup:
    $(WZZIP) $(BUILD)\developer\devsetup.zip  $(BACKUPDEFAULTS) setup.exe

test-manifest:
    # test that (a) linked manifest exists and correct, and (b) has uiAccess=true
    $(MT) -nologo -inputresource:$(PROGRAM)\developer\setup.exe -validate_manifest

install:
    @rem

!include ..\..\Target.mak
