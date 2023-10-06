#
# Keyman Developer Server Makefile
#

!include ..\Defines.mak

build: .virtual
    $(GIT_BASH_FOR_KEYMAN) build.sh --production --test

clean: .virtual
#TODO: move to build.sh `clean` target
    -rd /s/q build
    -rd /s/q dist
    -del tsconfig.tsbuildinfo

signcode:
    $(SIGNCODE) /d "Keyman" build\src\win32\console\node-hide-console-window.node
    $(SIGNCODE) /d "Keyman" build\src\win32\console\node-hide-console-window.x64.node
    $(SIGNCODE) /d "Keyman" build\src\win32\trayicon\addon.node
    $(SIGNCODE) /d "Keyman" build\src\win32\trayicon\addon.x64.node

wrap-symbols:
    @rem nothing to do

test-manifest:
    @rem This target needed as dependency for KMDECOMP

install:
    @rem nothing to do

!include ..\Target.mak



