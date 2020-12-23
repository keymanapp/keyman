#
# insthelp Makefile
#

!include ..\..\Defines.mak

build: version.res dirs
    $(DCC32) insthelp.dpr
    $(SENTRYTOOL_DELPHIPREP) insthelp.exe -dpr insthelp.dpr
    $(TDS2DBG) insthelp.exe
    $(COPY) insthelp.exe $(ROOT)\bin\desktop\insthelp.exe

test-manifest:
    # test that (a) linked manifest exists and correct, and (b) has uiAccess=true
    @rem $(MT) -nologo -inputresource:$(PROGRAM)\desktop\insthelp.exe -validate_manifest
    # TODO: Investigate why no manifest included?

clean: def-clean

signcode:
    $(SIGNCODE) /d "Keyman Install Helper" $(ROOT)\bin\desktop\insthelp.exe

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\desktop\insthelp.exe /t keyman-windows
    $(SYMSTORE) $(DEBUGPATH)\desktop\insthelp.dbg /t keyman-windows

backup:
    $(WZZIP) $(BUILD)\desktop\insthelp.zip  $(BACKUPDEFAULTS) insthelp.exe

install:
    rem

!include ..\..\Target.mak
