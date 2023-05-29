#
# Keyman Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res keymanmenuitem.res icons.res osktoolbar.res dirs
    rc langswitch\langswitchmanager.rc
    $(DELPHI_MSBUILD) keyman.dproj "/p:Platform=Win32"
#    $(DCC32) keyman.dpr
    $(SENTRYTOOL_DELPHIPREP) $(WIN32_TARGET_PATH)\keyman.exe -dpr keyman.dpr
    $(TDS2DBG) $(WIN32_TARGET_PATH)\keyman.exe
    $(COPY) $(WIN32_TARGET_PATH)\keyman.exe $(PROGRAM)\engine
    if exist $(WIN32_TARGET_PATH)\keyman.dbg $(COPY) $(WIN32_TARGET_PATH)\keyman.dbg $(DEBUGPATH)\engine

clean: def-clean

signcode:
    $(SIGNCODE) /d "Keyman Engine" $(PROGRAM)\engine\keyman.exe
# Sign the Sentry executables and libraries here
    $(SIGNCODE) /d "Keyman Engine" $(PROGRAM)\engine\sentry.dll
    $(SIGNCODE) /d "Keyman Engine" $(PROGRAM)\engine\sentry.x64.dll
    $(SIGNCODE) /d "Keyman Engine" $(PROGRAM)\engine\crashpad_handler.exe

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\engine\keyman.dll /t keyman-engine-windows
    $(SYMSTORE) $(DEBUGPATH)\engine\keyman.dbg /t keyman-engine-windows

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
    findstr "uiAccess=\"true\"" manifest.in >nul

test-manifest:
# test that (a) linked manifest exists and correct, and (b) has uiAccess=true
    $(MT) -nologo -inputresource:$(PROGRAM)\engine\keyman.exe -validate_manifest
    $(MT) -nologo -inputresource:$(PROGRAM)\engine\keyman.exe -out:temp.manifest
    findstr "uiAccess=\"true\"" temp.manifest >nul
    -del temp.manifest

!include ..\..\Target.mak
