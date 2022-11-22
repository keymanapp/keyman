#
# Test the PackageInfo file format read and write - xml and inf
#

!include ..\..\..\Defines.mak

test: build
    $(WIN32_TARGET_PATH)\packageinfotestsuite.exe -b -exit:continue

build:
    $(DELPHI_MSBUILD) "/p:CI=CI" packageinfotestsuite.dproj

clean: def-clean
    -del test\*.out.*

!include ..\..\..\Target.mak
