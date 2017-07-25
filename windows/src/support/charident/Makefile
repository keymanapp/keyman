#
# charident Makefile
#

!include ..\..\Defines.mak

build: version.res dirs
    $(DCC32) charident.dpr
    $(COPY) charident.exe $(PROGRAM)\support

clean: def-clean

signcode:
    $(SIGNCODE) /d "Character Identifier" $(PROGRAM)\support\charident.exe

backup:
    $(WZZIP) $(BUILD)\online\charident.zip  $(BACKUPDEFAULTS) charident.exe

!include ..\..\Target.mak
