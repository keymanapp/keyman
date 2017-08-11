#
# windowinfo Makefile
#

!include ..\..\Defines.mak

install: dirs build
    $(COPY) windowinfo.exe $(PROGRAM)\bldutil

build: version.res
    $(DCC32) windowinfo.dpr

clean: def-clean

!include ..\..\Target.mak
