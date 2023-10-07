#
# Keyman Developer Server Makefile
#

!include ..\Defines.mak

build: .virtual
    $(GIT_BASH_FOR_KEYMAN) build.sh configure build test installer

clean: .virtual
#TODO: move to build.sh `clean` target
    -rd /s/q build
    -rd /s/q dist
    -del tsconfig.tsbuildinfo

signcode:
    @rem nothing to do

wrap-symbols:
    @rem nothing to do

test-manifest:
    @rem This target needed as dependency for KMDECOMP

install:
    @rem nothing to do

!include ..\Target.mak



