#
# Sentrytool Makefile
#

!include ..\..\Defines.mak

build: dirs
    $(DELPHI_MSBUILD) sentrytool.dproj "/p:Platform=Win32"
    $(COPY) $(WIN32_TARGET_PATH)\sentrytool.exe $(PROGRAM)\buildtools

clean: def-clean

!include ..\..\Target.mak
