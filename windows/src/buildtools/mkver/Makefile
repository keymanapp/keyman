#
# Mkver Makefile
#

EXCLUDEPATHDEFINES=1

!include ..\..\Defines.mak

build: dirs
    $(DELPHI_MSBUILD) mkver.dproj "/p:Platform=Win32"
    $(COPY) $(WIN32_TARGET_PATH)\mkver.exe $(PROGRAM)\buildtools

clean: def-clean

!include ..\..\Target.mak
