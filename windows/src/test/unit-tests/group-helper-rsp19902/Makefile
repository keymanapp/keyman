#
# Test RSP-19902 workaround
#

!include ..\..\..\Defines.mak

test: build
    -$(WIN32_TARGET_PATH)\RegExGroupHelperRSP19902TestSuite.exe -b -exit:continue
  
build: 
    $(DELPHI_MSBUILD) RegExGroupHelperRSP19902TestSuite.dproj

clean: def-clean
    -rd /s/q Win32

!include ..\..\..\Target.mak
