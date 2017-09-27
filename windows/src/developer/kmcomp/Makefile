#
# Kmcomp Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res dirs icons
    $(DCC32) kmcomp.dpr
    $(TDS2DBG) kmcomp.exe 
    $(MAKEJCLDBG) kmcomp.map
    $(COPY) kmcomp.exe $(PROGRAM)\developer
    if exist kmcomp.dbg $(COPY) kmcomp.dbg $(DEBUGPATH)\developer

icons:
    rc icons.rc

clean: def-clean
    if exist icons.res del icons.res

signcode:
    $(SIGNCODE) /d "Keyman Developer Command-Line Compiler" $(PROGRAM)\developer\kmcomp.exe

backup:
    $(WZZIP) $(BUILD)\developer\kmcomp.zip  $(BACKUPDEFAULTS) kmcomp.exe

test-manifest:
    # test that (a) linked manifest exists and correct, and (b) has uiAccess=true
    $(MT) -nologo -inputresource:$(PROGRAM)\developer\kmcomp.exe -validate_manifest

install:
    $(COPY) $(PROGRAM)\developer\kmcomp.exe "$(INSTALLPATH_KEYMANDEVELOPER)\kmcomp.exe"

!include ..\..\Target.mak
