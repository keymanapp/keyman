#
# mbcolor Makefile (Tavultesoft)
#

!include ..\..\Defines.mak

build: dirs
    @-mkdir obj\Win32\$(TARGET_PATH)
    $(DCC32DPK) mbColorLibD10.dpk
    $(MAKE) install

install:
    $(DEVTOOLS) -ip $(OUTLIB)\mbColorLibD10.bpl
    $(DEVTOOLS) -ai $(DEVELOPER_ROOT)\src\ext\mbcolor

clean: def-clean

!include ..\..\Target.mak
