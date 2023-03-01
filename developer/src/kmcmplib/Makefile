#
# KMCmpLib Makefile
#

!include ..\Defines.mak

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

build: .virtual
    $(CALLER) ./build.sh clean:x86 configure:x86 build:x86 clean:x64 configure:x64 build:x64 $(BUILD_DEBUG)

clean:
    $(CALLER) ./build.sh clean:x86 clean:x64

signcode:
    @rem Not required

wrap-symbols:
    @rem Not required

test-manifest:
    @rem This target needed as dependency for TIKE and KMCMPDLL

install:
    @rem Not required

!include ..\Target.mak
