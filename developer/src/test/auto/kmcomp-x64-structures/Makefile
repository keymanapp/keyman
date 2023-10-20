#
# Test that struct sizes in tike and kmcmplib match
# for each of x86 and x64
#

!include ..\..\..\Defines.mak

test: build
    passtructsize\$(WIN32_TARGET_PATH)\passtructsize.exe > 32-passtructsize.txt
    cppstructsize\$(WIN32_TARGET_PATH)\cppstructsize.exe > 32-cppstructsize.txt
    echo Comparing 32-bit sizes
    fc 32-passtructsize.txt 32-cppstructsize.txt

    passtructsize\$(WIN64_TARGET_PATH)\passtructsize.exe > 64-passtructsize.txt
    cppstructsize\$(X64_TARGET_PATH)\cppstructsize.exe > 64-cppstructsize.txt
    echo Comparing 64-bit sizes
    fc 64-passtructsize.txt 64-cppstructsize.txt

build:
    $(DELPHI_MSBUILD) passtructsize\passtructsize.dproj "/p:Platform=Win32"
    $(DELPHI_MSBUILD) passtructsize\passtructsize.dproj "/p:Platform=Win64"
    $(MSBUILD) $(MSBUILD_BUILD) cppstructsize\cppstructsize.sln "/p:Platform=Win32"
    $(MSBUILD) $(MSBUILD_BUILD) cppstructsize\cppstructsize.sln "/p:Platform=x64"

clean: def-clean
    -rd /s/q Win32
    -rd /s/q Win64
    -rd /s/q x64
    -del *structsize.txt

!include ..\..\..\Target.mak
