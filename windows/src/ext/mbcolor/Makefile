#
# mbcolor Makefile (Tavultesoft)
#

!include ..\..\Defines.mak

build: dirs
    $(DCC32DPK) mbColorLibD10.dpk
    $(MAKE) install

install:
    $(DEVTOOLS) -ip $(OUTLIB)\mbColorLibD10.bpl
    $(DEVTOOLS) -ai $(EXT)\mbcolor

clean: def-clean

!include ..\..\Target.mak
