#
# KMCOMAPI Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res kbd_noicon.res dirs
    gentlb -Tkmcomapi.tlb kmcomapi.ridl
    $(DELPHI_MSBUILD) kmcomapi.dproj /p:Platform=Win32

    $(SENTRYTOOL_DELPHIPREP) $(WIN32_TARGET_PATH)\kmcomapi.dll -dpr kmcomapi.dpr
    $(TDS2DBG) $(WIN32_TARGET_PATH)\kmcomapi.dll
    $(COPY) $(WIN32_TARGET_PATH)\kmcomapi.dll $(PROGRAM)\engine
    if exist $(WIN32_TARGET_PATH)\kmcomapi.dbg $(COPY) $(WIN32_TARGET_PATH)\kmcomapi.dbg $(DEBUGPATH)\engine

    $(DELPHI_MSBUILD) kmcomapi.dproj /p:Platform=Win64

    if exist $(WIN64_TARGET_PATH)\kmcomapi.x64.dll del $(WIN64_TARGET_PATH)\kmcomapi.x64.dll
    # Delphi does not allow us to build to a different target filename so we rename after build
    ren $(WIN64_TARGET_PATH)\kmcomapi.dll kmcomapi.x64.dll
    # Not yet supported: $(SENTRYTOOL_DELPHIPREP) $(WIN64_TARGET_PATH)\kmcomapi.x64.dll -dpr kmcomapi.dpr
    $(COPY) $(WIN64_TARGET_PATH)\kmcomapi.x64.dll $(PROGRAM)\engine\kmcomapi.x64.dll
    if exist $(WIN64_TARGET_PATH)\kmcomapi.dbg $(COPY) $(WIN64_TARGET_PATH)\kmcomapi.dbg $(DEBUGPATH)\engine\kmcomapi.x64.dbg

kbd_noicon.res: kbd_noicon.rc
    rc $?

clean: def-clean
    -del kmcomapi.dll
    -del kmcomapi.tlb
    -rd /s/q Win32
    -rd /s/q Win64

signcode:
    $(SIGNCODE) /d "Keyman Engine" $(PROGRAM)\engine\kmcomapi.dll
    $(SIGNCODE) /d "Keyman Engine" $(PROGRAM)\engine\kmcomapi.x64.dll

install:
    cmd /c regsvr32 /s /u "$(INSTALLPATH_KEYMANENGINE)\kmcomapi.dll"
    $(COPY) $(PROGRAM)\engine\kmcomapi.dll "$(INSTALLPATH_KEYMANENGINE)"
    cmd /c regsvr32 /s "$(INSTALLPATH_KEYMANENGINE)\kmcomapi.dll"

    cmd /c regsvr32 /s /u "$(INSTALLPATH_KEYMANENGINE)\kmcomapi.x64.dll"
    $(COPY) $(PROGRAM)\engine\kmcomapi.x64.dll "$(INSTALLPATH_KEYMANENGINE)"
    cmd /c regsvr32 /s "$(INSTALLPATH_KEYMANENGINE)\kmcomapi.x64.dll"

backup:
    $(WZZIP) $(BUILD)\engine\kmcomapi.zip  $(BACKUPDEFAULTS) $(WIN32_TARGET_PATH)\kmcomapi.dll $(WIN64_TARGET_PATH)\kmcomapi.x64.dll $(WIN32_TARGET_PATH)\kmcomapi.dbg

!include ..\..\Target.mak
