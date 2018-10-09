#
# Test shared buffer data size is identical on all languages and platforms
#

!include ..\..\..\Defines.mak

test: build
    delphi\$(WIN32_TARGET_PATH)\delphishareddata.exe > delphi-win32.txt
    delphi\$(WIN64_TARGET_PATH)\delphishareddata.exe > delphi-win64.txt
    cpp\$(TARGET_PATH)\cppshareddata.exe > cpp-win32.txt
    cpp\x64\$(TARGET_PATH)\cppshareddata.exe > cpp-win64.txt  
    fc delphi-win32.txt delphi-win64.txt
    fc delphi-win32.txt cpp-win32.txt
    fc delphi-win32.txt cpp-win64.txt
build:
    cd delphi
    $(MAKE) build
    cd ..\cpp
    $(MAKE) build
    cd ..

clean: def-clean
    -del delphi-win32.txt
    -del delphi-win64.txt
    -del cpp-win32.txt
    -del cpp-win64.txt
    cd delphi
    $(MAKE) clean
    cd ..\cpp
    $(MAKE) clean
    cd ..

!include ..\..\..\Target.mak
