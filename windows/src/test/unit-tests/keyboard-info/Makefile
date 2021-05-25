#
# Test that .keyboard_info validation works correctly
#
# Uses kmcomp.exe rather than attempting to run a separate test
#

!include ..\..\..\Defines.mak

test: build
    test.bat "$(PROGRAM)\developer\kmcomp.exe"

build:
    rem Nothing to do

clean: def-clean
    -rd /s/q Win32

!include ..\..\..\Target.mak
