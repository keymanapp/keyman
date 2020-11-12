#
# Keyman Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res keymanmenuitem.res icons.res osktoolbar.res dirs
    rc langswitch\langswitchmanager.rc
    $(DCC32) keyman.dpr
    $(SENTRYTOOL_DELPHIPREP) keyman.exe -dpr keyman.dpr
    $(TDS2DBG) keyman.exe
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
    copy manifest.in std-manifest.tmp
    copy debug-manifest.in manifest.in
    $(MAKE) manifest.res
    copy std-manifest.tmp manifest.in
    -del std-manifest.tmp

install:
    $(COPY) $(PROGRAM)\engine\keyman.exe "$(INSTALLPATH_KEYMANENGINE)"

test-uiaccess:
    grep -c "uiAccess=\"true\"" manifest.in

test-manifest:
    # test that (a) linked manifest exists and correct, and (b) has uiAccess=true
    $(MT) -nologo -inputresource:$(PROGRAM)\engine\keyman.exe -validate_manifest
    $(MT) -nologo -inputresource:$(PROGRAM)\engine\keyman.exe -out:temp.manifest
    grep -c "uiAccess=\"true\"" temp.manifest
    -del temp.manifest

!include ..\..\Target.mak
