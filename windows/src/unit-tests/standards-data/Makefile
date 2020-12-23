#
# Test the standards data reading
#

!include ..\..\Defines.mak

test: build
    -$(WIN32_TARGET_PATH)\standardsdata.exe -b -exit:continue
  
build: 
    $(DELPHI_MSBUILD) /p:CI=CI standardsdata.dproj

clean: def-clean
    -rd /s/q Win32

!include ..\..\Target.mak
