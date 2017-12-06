#
# Test the PackageInfo file format read and write - xml and inf
#

!include ..\..\Defines.mak

test: build
    Win32\Debug\packageinfotestsuite.exe -b
  
build: 
    $(DELPHI_MSBUILD) /p:CI=CI packageinfotestsuite.dproj

clean: def-clean
    -rd /s/q Win32
    -del dunitx-results.xml
    -del test\*.out.*

!include ..\..\Target.mak
