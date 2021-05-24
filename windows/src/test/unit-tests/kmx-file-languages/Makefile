#
# Test the read of language info from kmx files and translation
# from legacy ids to BCP47
#
# NOTE: The .dproj needs $(CI) added to the Delphi Compiler/Conditional defines (All
# configurations - all platforms) section in order for the CI flag to be passed in.
# (It's best to make this change in Delphi IDE).
#

!include ..\..\..\Defines.mak

test: build
    -$(WIN32_TARGET_PATH)\kmxfilelanguagestestsuite.exe -b -exit:continue

build:
    $(DELPHI_MSBUILD) /p:CI=CI kmxfilelanguagestestsuite.dproj

clean: def-clean
    -rd /s/q Win32

!include ..\..\..\Target.mak
