#
# Test androidstringtokeymanlocalestring
#

!include ..\..\..\Defines.mak

test: build
    -$(WIN32_TARGET_PATH)\androidstringtokeymanlocalestring.exe -b -exit:continue

build:
    $(DELPHI_MSBUILD) androidstringtokeymanlocalestring.dproj

clean: def-clean
    -rd /s/q Win32

!include ..\..\..\Target.mak
