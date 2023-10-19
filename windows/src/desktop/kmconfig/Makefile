#
# KMConfig Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res
    $(DELPHI_MSBUILD) kmconfig.dproj "/p:Platform=Win32"
    $(SENTRYTOOL_DELPHIPREP) $(WIN32_TARGET_PATH)\kmconfig.exe -dpr kmconfig.dpr
    $(TDS2DBG) $(WIN32_TARGET_PATH)\kmconfig.exe
    $(COPY) $(WIN32_TARGET_PATH)\kmconfig.exe $(PROGRAM)\desktop
    if exist $(WIN32_TARGET_PATH)\kmconfig.dbg $(COPY) $(WIN32_TARGET_PATH)\kmconfig.dbg $(DEBUGPATH)\desktop

clean: def-clean

signcode:
    $(SIGNCODE) /d "Keyman for Windows" $(PROGRAM)\desktop\kmconfig.exe

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\desktop\kmconfig.exe /t keyman-windows
    $(SYMSTORE) $(DEBUGPATH)\desktop\kmconfig.dbg /t keyman-windows

test-manifest:
# test that linked manifest exists and correct
    $(MT) -nologo -inputresource:$(PROGRAM)\desktop\kmconfig.exe -validate_manifest

install:
    copy $(ROOT)\bin\desktop\kmconfig.exe "$(INSTALLPATH_KEYMANDESKTOP)"

!include ..\..\Target.mak
