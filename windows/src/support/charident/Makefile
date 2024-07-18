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

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\support\charident.exe /t keyman-windows
#TODO: $(SYMSTORE) $(DEBUGPATH)\support\charident.dbg /t keyman-windows

!include ..\..\Target.mak
