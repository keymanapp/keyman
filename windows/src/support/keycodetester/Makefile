#
# Keycodetester Makefile
#

!include ..\..\Defines.mak

install: dirs build
    $(COPY) keycodetester.exe $(PROGRAM)\bldutil

build:
    $(DCC32) keycodetester.dpr

clean: def-clean

!include ..\..\Target.mak
