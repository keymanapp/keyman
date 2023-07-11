#
# Keyman32-ver Makefile
#

!include ..\..\Defines.mak

build: version.res dirs pull-core
   $(MKVER_U) premake.in premake.mak
	 $(MKVER_U) $(ROOT)\src\engine\keyman32-ver\keyman64-build-version.in $(ROOT)\src\engine\keyman32-ver\keyman64-build-version.h
	 $(MKVER_U) keyman32-ver.in keyman32-ver.def
	 $(MAKE) -fpremake.mak build

clean: def-clean
    $(MAKE) -fpremake.mak clean

signcode:
    $(MKVER_U) premake.in premake.mak
    $(MAKE) -fpremake.mak signcode

wrap-symbols:
    $(MKVER_U) premake.in premake.mak
    $(MAKE) -fpremake.mak wrap-symbols

install:
    $(MKVER_U) premake.in premake.mak
    $(MAKE) -fpremake.mak install

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
    cd $(ROOT)\src\engine\keyman32-ver

!include ..\..\Target.mak
