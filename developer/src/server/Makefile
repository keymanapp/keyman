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
    $(SIGNCODE) /d "Keyman Developer" $(DEVELOPER_PROGRAM)\server\dist\win32\console\node-hide-console-window.node
    $(SIGNCODE) /d "Keyman Developer" $(DEVELOPER_PROGRAM)\server\dist\win32\console\node-hide-console-window.x64.node
    $(SIGNCODE) /d "Keyman Developer" $(DEVELOPER_PROGRAM)\server\dist\win32\trayicon\addon.node
    $(SIGNCODE) /d "Keyman Developer" $(DEVELOPER_PROGRAM)\server\dist\win32\trayicon\addon.x64.node

wrap-symbols:
    @rem nothing to do

test-manifest:
    @rem This target needed as dependency for KMDECOMP

install:
    @rem nothing to do

!include ..\Target.mak



