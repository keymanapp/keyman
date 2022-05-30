#
# buildunidata Makefile
#

!include ..\..\Defines.mak

build: dirs version.res
    $(DELPHI_MSBUILD) buildunidata.dproj "/p:Platform=Win32"
    $(COPY) $(WIN32_TARGET_PATH)\buildunidata.exe $(PROGRAM)\buildtools

run:
    $(PROGRAM)\buildtools\buildunidata $(ROOT)\src\global\inst\data $(ROOT)\src\global\inst\data\UnicodeData.mdb

clean: def-clean

!include ..\..\Target.mak
