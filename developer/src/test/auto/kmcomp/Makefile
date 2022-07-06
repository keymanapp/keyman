#
# Test that kmcomp passes various compile tests
#
# Uses kmcomp.exe rather than attempting to run a separate test
#

!include ..\..\..\Defines.mak

test: build
    test.bat -c "$(DEVELOPER_PROGRAM)\kmcomp.exe"

build:
    rem Nothing to do

clean: def-clean
    -del *.kmx
    -del *.js
    -del *.kmp
    -del *.kpj.user

!include ..\..\..\Target.mak
