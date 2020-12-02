#
# Test various components of Keyman for Windows Setup
#
# NOTE: The .dproj needs $(CI) added to the Delphi Compiler/Conditional defines (All
# configurations - all platforms) section in order for the CI flag to be passed in.
# (It's best to make this change in Delphi IDE).
#

!include ..\..\Defines.mak

test: build
    -$(WIN32_TARGET_PATH)\WindowsSetupTest.exe -b -exit:continue

build:
    $(DELPHI_MSBUILD) /p:CI=CI WindowsSetupTest.dproj

clean: def-clean
    -rd /s/q Win32

!include ..\..\Target.mak
