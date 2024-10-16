#
# tscrmlink Makefile
#

!include ..\..\Defines.mak

build: version.res dirs
    $(DCC32) tscrmlink.dpr
    $(COPY) tscrmlink.exe $(PROGRAM)\online

clean: def-clean

signcode:
    $(SIGNCODE) /d "Tavultesoft Keyman CRM Client" $(PROGRAM)\online\tscrmlink.exe

backup:
    $(WZZIP) $(BUILD)\online\tscrmlink.zip  $(BACKUPDEFAULTS) tscrmlink.exe

!include ..\..\Target.mak
