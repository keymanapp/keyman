#
# Integration tests for keymanapi
#

!include ..\..\..\Defines.mak

!ifdef EXCLUDE_ELEVATED
TEST_MODE=--exclude:Elevated
!endif

test: build
    $(WIN32_TARGET_PATH)\keymanapi_test.exe -b -exit:continue $(TEST_MODE)
    $(WIN64_TARGET_PATH)\keymanapi_test.exe -b -exit:continue $(TEST_MODE)
  
build: 
    $(DELPHI_MSBUILD) /p:Platform=Win32 keymanapi_test.dproj
    $(DELPHI_MSBUILD) /p:Platform=Win64 keymanapi_test.dproj

clean: def-clean
    -rd /s/q Win32
    -rd /s/q Win64
    
!include ..\..\..\Target.mak
