#
# TSysInfo Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res dirs
    cd ..\tsysinfox64

!ifdef SIGNCODE_BUILD
    $(MAKE) signcode   # have to sign x64 version here as otherwise signcode is done after build.
!endif

    cd ..\tsysinfo
    $(COPY) $(PROGRAM)\engine\tsysinfox64.exe tsysinfox64.bin
    rc tsysinfo_x64.rc
    del tsysinfox64.bin

    $(DELPHI_MSBUILD) tsysinfo.dproj /p:Platform=Win32
    $(SENTRYTOOL_DELPHIPREP) $(WIN32_TARGET_PATH)\tsysinfo.exe -dpr tsysinfo.dpr
    $(TDS2DBG) $(WIN32_TARGET_PATH)\tsysinfo.exe
    $(COPY) $(WIN32_TARGET_PATH)\tsysinfo.exe $(PROGRAM)\engine
    if exist $(WIN32_TARGET_PATH)\tsysinfo.dbg $(COPY) $(WIN32_TARGET_PATH)\tsysinfo.dbg $(DEBUGPATH)\engine

clean: def-clean
    -del tsysinfox64.bin

signcode:
    $(SIGNCODE) /d "Keyman Engine Diagnostics" $(PROGRAM)\engine\tsysinfo.exe

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\engine\tsysinfo.exe /t keyman-engine-windows
    $(SYMSTORE) $(DEBUGPATH)\engine\tsysinfo.dbg /t keyman-engine-windows

install:
    $(COPY) $(PROGRAM)\engine\tsysinfo.exe "$(INSTALLPATH_KEYMANENGINE)"
    $(COPY) $(ROOT)\src\engine\tsysinfo\*.xsl "$(INSTALLPATH_KEYMANENGINE)"

test-manifest:
    # test that linked manifest exists and correct
    $(MT) -nologo -inputresource:$(PROGRAM)\engine\tsysinfo.exe -validate_manifest

!include ..\..\Target.mak
