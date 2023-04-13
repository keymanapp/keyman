#
# Keyman32 Makefile
#

!include ..\..\Defines.mak

build: version.res dirs
    $(MKVER_U) exports.in exports.h
    $(MSBUILD) keyman32.vcxproj $(MSBUILD_BUILD)
    $(COPY) $(WIN32_TARGET_PATH)\keyman32.dll $(PROGRAM)\engine
    $(COPY) $(WIN32_TARGET_PATH)\keyman32.pdb $(DEBUGPATH)\engine
#   $(COPY) $(WIN32_TARGET_PATH)\keyman32.lib $(OUTLIB) done by vcxproj

clean: def-clean
    $(MSBUILD) $(MSBUILD_CLEAN) keyman32.vcxproj

signcode:
    $(SIGNCODE) /d "Keyman Engine" $(PROGRAM)\engine\keyman32.dll

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\engine\keyman32.dll /t keyman-engine-windows
    $(SYMSTORE) $(DEBUGPATH)\engine\keyman32.pdb /t keyman-engine-windows

install:
    $(COPY) $(PROGRAM)\engine\keyman32.dll "$(INSTALLPATH_KEYMANENGINE)\keyman32.dll"
    echo You may want to manually tweak keyman-debug-etw.man and fill in $(INSTALLPATH_KEYMANENGINE)\keyman32.dll
    echo and then run wevtutil im keyman-debug-etw.man to get the latest event tracing

!ifdef DEBUG
CORE_DEBUG=--debug
MESON_TARGET=debug
!else
MESON_TARGET=release
!endif

!ifdef GIT_BASH_FOR_KEYMAN
CALLER=$(GIT_BASH_FOR_KEYMAN)
!else
CALLER=start /wait
!endif
!ifdef DEBUG
BUILD_DEBUG=-d
!else
BUILD_DEBUG=
!endif

pull-core:
    cd $(ROOT)\..\core
    $(CALLER) ./build.sh --no-tests clean:x86 configure:x86 build:x86 clean:x64 configure:x64 build:x64 $(BUILD_DEBUG)
    cd $(ROOT)\src\engine\keyman32

!include ..\..\Target.mak
