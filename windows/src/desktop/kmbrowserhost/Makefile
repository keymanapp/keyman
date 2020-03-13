#
# KMBrowserHost Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res dirs
    $(DELPHI_MSBUILD) kmbrowserhost.dproj /p:Platform=Win32

    $(TDS2DBG) $(WIN32_TARGET_PATH)\kmbrowserhost.exe
    $(SENTRYTOOL_DELPHIPREP) $(WIN32_TARGET_PATH)\kmbrowserhost.exe -dpr kmbrowserhost.dpr
    $(COPY) $(WIN32_TARGET_PATH)\kmbrowserhost.exe $(PROGRAM)\desktop
    if exist $(WIN32_TARGET_PATH)\kmbrowserhost.dbg $(COPY) $(WIN32_TARGET_PATH)\kmbrowserhost.dbg $(DEBUGPATH)\desktop

clean: def-clean
    -del kmbrowserhost.exe
    -rd /s/q Win32
    -rd /s/q Win64

signcode:
    $(SIGNCODE) /d "Keyman Desktop" $(PROGRAM)\desktop\kmbrowserhost.exe

install:
    $(COPY) $(PROGRAM)\desktop\kmbrowserhost.exe "$(INSTALLPATH_KEYMANDESKTOP)"

backup:
    $(WZZIP) $(BUILD)\desktop\kmbrowserhost.zip  $(BACKUPDEFAULTS) $(WIN32_TARGET_PATH)\kmbrowserhost.exe $(WIN32_TARGET_PATH)\kmbrowserhost.dbg

!include ..\..\Target.mak
