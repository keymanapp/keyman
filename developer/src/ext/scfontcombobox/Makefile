#
# scFontCombo Makefile
#

!include ..\..\Defines.mak

build: dirs
    @-mkdir obj\Win32\$(TARGET_PATH)
    $(DCC32DPK) scFontCombo.dpk
    $(MAKE) install

install:
    $(DEVTOOLS) -ip $(OUTLIB)\scFontCombo.bpl
    $(DEVTOOLS) -ai $(DEVELOPER_ROOT)\src\ext\scfontcombobox

clean: def-clean

!include ..\..\Target.mak
