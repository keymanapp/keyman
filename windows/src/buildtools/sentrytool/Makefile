#
# Sentrytool Makefile
#

!include ..\..\Defines.mak

build: dirs
    # build with DCC32PATH as $DCC32 command uses devtools.exe...
    # $(DCC32) devtools.dpr
    $(DELPHI_MSBUILD) sentrytool.dproj /p:Platform=Win32
    $(COPY) $(WIN32_TARGET_PATH)\sentrytool.exe $(PROGRAM)\buildtools

clean: def-clean

!include ..\..\Target.mak
