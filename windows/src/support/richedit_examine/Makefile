#
# richedit_examine Makefile
#

!include ..\..\Defines.mak

install: dirs build
    $(COPY) richedit_examine.exe $(PROGRAM)\bldutil

build: version.res
    $(DCC32) richedit_examine.dpr

clean: def-clean

!include ..\..\Target.mak
