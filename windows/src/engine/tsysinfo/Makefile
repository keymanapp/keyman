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

    $(DCC32) tsysinfo.dpr
    $(SENTRYTOOL_DELPHIPREP) tsysinfo.exe -dpr tsysinfo.dpr
    $(TDS2DBG) tsysinfo.exe
    $(COPY) tsysinfo.exe $(PROGRAM)\engine
    if exist tsysinfo.dbg $(COPY) tsysinfo.dbg $(DEBUGPATH)\engine

clean: def-clean
    -del tsysinfox64.bin

signcode:
    $(SIGNCODE) /d "Keyman Engine Diagnostics" $(PROGRAM)\engine\tsysinfo.exe

backup:
    $(WZZIP) $(BUILD)\engine\tsysinfo.zip  $(BACKUPDEFAULTS) tsysinfo.exe

install:
    $(COPY) $(PROGRAM)\engine\tsysinfo.exe "$(INSTALLPATH_KEYMANENGINE)"
    $(COPY) $(ROOT)\src\engine\tsysinfo\*.xsl "$(INSTALLPATH_KEYMANENGINE)"

test-manifest:
    # test that linked manifest exists and correct
    $(MT) -nologo -inputresource:$(PROGRAM)\engine\tsysinfo.exe -validate_manifest

!include ..\..\Target.mak
