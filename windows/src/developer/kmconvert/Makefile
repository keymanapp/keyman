#
# Kmconvert Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res dirs icons
    $(DCC32) kmconvert.dpr
    $(TDS2DBG) kmconvert.exe 
    $(MAKEJCLDBG) kmconvert.map
    $(COPY) kmconvert.exe $(PROGRAM)\developer
    if exist kmconvert.dbg $(COPY) kmconvert.dbg $(DEBUGPATH)\developer

icons:
    rc icons.rc

clean: def-clean
    if exist icons.res del icons.res

signcode:
    $(SIGNCODE) /d "Keyman Developer Conversion Utility" $(PROGRAM)\developer\kmconvert.exe

backup:
    $(WZZIP) $(BUILD)\developer\kmconvert.zip  $(BACKUPDEFAULTS) kmconvert.exe

test-manifest:
    # test that (a) linked manifest exists and correct
    $(MT) -nologo -inputresource:$(PROGRAM)\developer\kmconvert.exe -validate_manifest

install:
    $(COPY) $(PROGRAM)\developer\kmconvert.exe "$(INSTALLPATH_KEYMANDEVELOPER)\kmconvert.exe"

!include ..\..\Target.mak
