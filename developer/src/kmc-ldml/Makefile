#
# Keyman Developer - kmc Keyboard Compiler Makefile
#

!include ..\Defines.mak

# We do configure here because parent Makefile calls this first; other
# kmc and kmc-* makefiles don't do it
build: configure .virtual
    $(GIT_BASH_FOR_KEYMAN) build.sh build

configure: .virtual
    $(GIT_BASH_FOR_KEYMAN) build.sh configure

clean: .virtual
    $(GIT_BASH_FOR_KEYMAN) build.sh clean

test: .virtual
    $(GIT_BASH_FOR_KEYMAN) build.sh test

# build.sh bundle must be run from shell as it requires a temp folder to be
# passed in. See inst/download.in.mak for instantiation.

publish: .virtual
    $(GIT_BASH_FOR_KEYMAN) build.sh publish

signcode:
    @rem nothing to do

wrap-symbols:
    @rem nothing to do

test-manifest:
    @rem nothing to do

install:
    @rem nothing to do

!include ..\Target.mak
