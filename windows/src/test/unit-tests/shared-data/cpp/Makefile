#
# shared-data C++ unit test
#

!include ..\..\..\..\Defines.mak

build: 
    $(MSBUILD) $(MSBUILD_BUILD) /p:Platform=Win32 cppshareddata.vcxproj
    $(MSBUILD) $(MSBUILD_BUILD) /p:Platform=x64 cppshareddata.vcxproj

clean: def-clean
    $(MSBUILD) $(MSBUILD_CLEAN) cppshareddata.vcxproj
    -rd /s/q Win32
    -rd /s/q x64

!include ..\..\..\..\Target.mak
