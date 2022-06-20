#
# buildunidata Makefile
#

!include ..\..\Defines.mak

build:
    $(DELPHI_MSBUILD) buildunidata.dproj "/p:Platform=Win32"

run:
    $(WIN32_TARGET_PATH)\buildunidata.exe $(WINDOWS_ROOT)\src\global\inst\data $(WINDOWS_ROOT)\src\global\inst\data\UnicodeData.mdb

clean: def-clean

!include ..\..\Target.mak
