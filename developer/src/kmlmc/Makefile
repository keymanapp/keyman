#
# Keyman Developer - Lexical Model Compiler Makefile
#

!include ..\Defines.mak

build: .virtual
    $(GIT_BASH_FOR_KEYMAN) build.sh

clean: .virtual
#TODO: move to build.sh `clean` target
    -rd /s/q dist
    -rd /s/q dist-tests
    -del tsconfig.tsbuildinfo

test: .virtual
    $(GIT_BASH_FOR_KEYMAN) build.sh -tdd

signcode:
    @rem nothing to do

wrap-symbols:
    @rem nothing to do

test-manifest:
    @rem This target needed as dependency for KMDECOMP

install:
    @rem nothing to do

!include ..\Target.mak
