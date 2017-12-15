#
# Test that package compiler manages keyboard and package versions correctly
#
# NOTE: The .dproj needs $(CI) added to the Delphi Compiler/Conditional defines (All 
# configurations - all platforms) section in order for the CI flag to be passed in.
# (It's best to make this change in Delphi IDE). 
#

!include ..\..\Defines.mak

test: build
    Win32\Debug\keyboardpackageversionstestsuite.exe -b
  
build: 
    $(DELPHI_MSBUILD) /p:CI=CI keyboardpackageversionstestsuite.dproj

clean: def-clean
    -rd /s/q Win32
    -del *.kmp

!include ..\..\Target.mak
