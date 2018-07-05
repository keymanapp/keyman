#
# Cef4Delphi Makefile (Keyman)
#

!include ..\..\Defines.mak

build: dirs
    $(DCC32DPK) packages\CEF4Delphi.dpk
    $(MAKE) install

install:
    $(DEVTOOLS) -ip $(OUTLIB)\CEF4Delphi.bpl
    $(DEVTOOLS) -ai $(EXT)\cef4delphi\source

clean: def-clean
    $(CLEAN) packages\*.bpl
    $(CLEAN) packages\*.dcp

!include ..\..\Target.mak
