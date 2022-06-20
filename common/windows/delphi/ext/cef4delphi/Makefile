#
# Cef4Delphi Makefile (Keyman)
#

!include ..\..\Defines.mak

build:
    cd packages
    @-mkdir obj\Win32\$(TARGET_PATH)
    $(DCC32DPK) CEF4Delphi.dpk
    cd ..
    $(MAKE) install

install:
    $(DEVTOOLS) -ip $(OUTLIB)\CEF4Delphi.bpl
    $(DEVTOOLS) -ai $(KEYMAN_ROOT)\common\windows\delphi\ext\cef4delphi\source

clean: def-clean
    $(CLEAN) packages\*.bpl
    $(CLEAN) packages\*.dcp

!include ..\..\Target.mak
