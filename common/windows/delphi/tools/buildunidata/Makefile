#
# buildunidata Makefile
#

!include ..\..\Defines.mak

build:
    $(DELPHI_MSBUILD) buildunidata.dproj "/p:Platform=Win32"

run:
    -mkdir $(KEYMAN_ROOT)\common\windows\bin\data
    $(WIN32_TARGET_PATH)\buildunidata.exe $(KEYMAN_ROOT)\common\windows\data $(KEYMAN_ROOT)\common\windows\bin\data\unicodedata.mdb

clean: def-clean

!include ..\..\Target.mak
