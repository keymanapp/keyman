#
# Keyman Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res keymanmenuitem.res icons.res osktoolbar.res dirs
    rc langswitch\langswitchmanager.rc
    $(DCC32) keyman.dpr
    $(TDS2DBG) keyman.exe 
    $(MAKEJCLDBG) keyman.map
    $(COPY) keyman.exe $(PROGRAM)\engine
    if exist keyman.dbg $(COPY) keyman.dbg $(DEBUGPATH)\engine

clean: def-clean

signcode:
    $(SIGNCODE) /d "Keyman Engine" $(PROGRAM)\engine\keyman.exe

backup:
    $(WZZIP) $(BUILD)\engine\keyman.zip $(BACKUPDEFAULTS) keyman.exe

keymanmenuitem.res:
    rc keymanmenuitem.rc

icons.res:
    rc icons.rc

osktoolbar.res:
    rc osktoolbar.rc

debug-manifest:
    -del manifest.res
    copy manifest.xml std-manifest.xml
    copy debug-manifest.xml manifest.xml
    $(MAKE) manifest.res
    copy std-manifest.xml manifest.xml

install:
    $(COPY) $(PROGRAM)\engine\keyman.exe "C:\Program Files (x86)\Common Files\Keyman\Keyman Engine 10.0"
    $(COPY) keyman.rsm "C:\Program Files (x86)\Common Files\Keyman\Keyman Engine 10.0"

test-uiaccess:
    grep -c "uiAccess=\"true\"" manifest.xml

test-manifest:
    # test that (a) linked manifest exists and correct, and (b) has uiAccess=true
    $(MT) -nologo -inputresource:$(PROGRAM)\engine\keyman.exe -validate_manifest
    $(MT) -nologo -inputresource:$(PROGRAM)\engine\keyman.exe -out:temp.manifest
    grep -c "uiAccess=\"true\"" temp.manifest
    -del temp.manifest

!include ..\..\Target.mak
