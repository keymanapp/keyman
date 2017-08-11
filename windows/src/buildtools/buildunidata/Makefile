#
# buildunidata Makefile
#

!include ..\..\Defines.mak

build: dirs version.res
    $(DCC32) buildunidata.dpr
    $(COPY) buildunidata.exe $(PROGRAM)\buildtools

run:
    $(PROGRAM)\buildtools\buildunidata $(ROOT)\src\global\inst\data $(ROOT)\src\global\inst\data\UnicodeData.mdb

clean: def-clean

!include ..\..\Target.mak
