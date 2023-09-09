#
# Test that kmc passes various compile tests
#
# Uses kmc.exe rather than attempting to run a separate test
#

!include ..\..\..\Defines.mak

test: build
    test.bat -c $(DEVELOPER_PROGRAM)\kmc.cmd

build:
    rem Nothing to do

clean: def-clean
    -del *.kmx
    -del *.js
    -del *.kmp
    -del *.kpj.user

!include ..\..\..\Target.mak
