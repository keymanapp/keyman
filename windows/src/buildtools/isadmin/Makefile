#
# IsAdmin Makefile
#

!include ..\..\Defines.mak

build: dirs version.res
    $(DCC32) isadmin.dpr
    $(COPY) isadmin.exe $(PROGRAM)\buildtools

clean: def-clean

!include ..\..\Target.mak
