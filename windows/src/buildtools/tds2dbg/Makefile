#
# Convert delphi .tds into .dbg for WinDBG compatibility
#

!include ..\..\Defines.mak

build: clean
    cd $(ROOT)\src\ext\tds2dbg
    $(MAKE)
    
#    $(DCC32) tds2dbg.dpr
#    $(COPY) tds2dbg.exe $(PROGRAM)\buildtools
#    $(COPY) dbghelp.dll $(PROGRAM)\buildtools

clean: def-clean

!include ..\..\Target.mak
  