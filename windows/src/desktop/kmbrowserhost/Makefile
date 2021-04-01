#
# KMBrowserHost Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res dirs
    $(DELPHI_MSBUILD) kmbrowserhost.dproj /p:Platform=Win32

    $(SENTRYTOOL_DELPHIPREP) $(WIN32_TARGET_PATH)\kmbrowserhost.exe -dpr kmbrowserhost.dpr
    $(TDS2DBG) $(WIN32_TARGET_PATH)\kmbrowserhost.exe
    $(COPY) $(WIN32_TARGET_PATH)\kmbrowserhost.exe $(PROGRAM)\desktop
    if exist $(WIN32_TARGET_PATH)\kmbrowserhost.dbg $(COPY) $(WIN32_TARGET_PATH)\kmbrowserhost.dbg $(DEBUGPATH)\desktop

clean: def-clean
    -del kmbrowserhost.exe
    -rd /s/q Win32
    -rd /s/q Win64

signcode:
    $(SIGNCODE) /d "Keyman" $(PROGRAM)\desktop\kmbrowserhost.exe

install:
    $(COPY) $(PROGRAM)\desktop\kmbrowserhost.exe "$(INSTALLPATH_KEYMANDESKTOP)"

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\desktop\kmbrowserhost.exe /t keyman-windows
    $(SYMSTORE) $(DEBUGPATH)\desktop\kmbrowserhost.dbg /t keyman-windows

backup:
    $(WZZIP) $(BUILD)\desktop\kmbrowserhost.zip  $(BACKUPDEFAULTS) $(WIN32_TARGET_PATH)\kmbrowserhost.exe $(WIN32_TARGET_PATH)\kmbrowserhost.dbg

!include ..\..\Target.mak
