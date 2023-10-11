#
# KMShell Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res icons xml
    $(DELPHI_MSBUILD) kmshell.dproj "/p:Platform=Win32"
    $(SENTRYTOOL_DELPHIPREP) $(WIN32_TARGET_PATH)\kmshell.exe -dpr kmshell.dpr
    $(TDS2DBG) $(WIN32_TARGET_PATH)\kmshell.exe
    $(COPY) $(WIN32_TARGET_PATH)\kmshell.exe $(PROGRAM)\desktop
    if exist $(WIN32_TARGET_PATH)\kmshell.dbg $(COPY) $(WIN32_TARGET_PATH)\kmshell.dbg $(DEBUGPATH)\desktop

icons:
    rc icons.rc

xml: sentry-init.js
    cd $(ROOT)\src\desktop\kmshell\xml
    -del /q $(ROOT)\bin\desktop\xml\*
    copy * $(ROOT)\bin\desktop\xml
    cd $(ROOT)\src\desktop\kmshell
    -rmdir /S /Q $(ROOT)\bin\desktop\locale
    mkdir $(ROOT)\bin\desktop\locale
    xcopy /S /I $(ROOT)\src\desktop\kmshell\locale $(ROOT)\bin\desktop\locale\

sentry-init.js:
    cd $(ROOT)\src\desktop\kmshell\xml
    $(MKVER_U) sentry.init.js.in sentry.init.js

clean: def-clean
    if exist MessageIdentifiers.pas del MessageIdentifiers.pas
    if exist icons\icons.res del icons\icons.res

signcode:
    $(SIGNCODE) /d "Keyman" $(PROGRAM)\desktop\kmshell.exe

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\desktop\kmshell.exe /t keyman-windows
    $(SYMSTORE) $(DEBUGPATH)\desktop\kmshell.dbg /t keyman-windows

test-manifest:
# test that (a) linked manifest exists and correct, and (b) has uiAccess=true
    $(MT) -nologo -inputresource:$(PROGRAM)\desktop\kmshell.exe -validate_manifest

install: .virtual
    copy $(ROOT)\bin\desktop\kmshell.exe "$(INSTALLPATH_KEYMANDESKTOP)"
    copy $(ROOT)\bin\desktop\xml\* "$(INSTALLPATH_KEYMANDESKTOP)\xml\"

!include ..\..\Target.mak
