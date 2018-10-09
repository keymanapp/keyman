#
# shared-data Delphi unit test
#

!include ..\..\..\..\Defines.mak

build: 
    $(DELPHI_MSBUILD) /p:Platform=Win32 delphishareddata.dproj
    $(DELPHI_MSBUILD) /p:Platform=Win64 delphishareddata.dproj

clean: def-clean
    -rd /s/q Win32
    -rd /s/q Win64

!include ..\..\..\..\Target.mak
