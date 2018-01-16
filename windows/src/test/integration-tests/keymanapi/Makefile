#
# Integration tests for keymanapi
#

!include ..\..\..\Defines.mak

!ifdef EXCLUDE_ELEVATED
TEST_MODE=--exclude:Elevated
!endif

test: build
    $(WIN32_TARGET_PATH)\keymanapi_test.exe -b -exit:continue $(TEST_MODE)
!ifndef DELPHI_STARTER
    $(WIN64_TARGET_PATH)\keymanapi_test.exe -b -exit:continue $(TEST_MODE)
!endif
  
build: 
    $(DELPHI_MSBUILD) /p:Platform=Win32 keymanapi_test.dproj
!ifndef DELPHI_STARTER
    $(DELPHI_MSBUILD) /p:Platform=Win64 keymanapi_test.dproj
!endif

clean: def-clean
    -rd /s/q Win32
    -rd /s/q Win64
    
!include ..\..\..\Target.mak
