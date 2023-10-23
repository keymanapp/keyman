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
    $(CALLER) ./build.sh clean configure build $(BUILD_DEBUG)

clean:
    $(CALLER) ./build.sh clean $(BUILD_DEBUG)

signcode:
    @rem Not required

wrap-symbols:
    @rem Not required

test-manifest:
    @rem This target needed as dependency for TIKE

test:
    $(CALLER) ./build.sh test $(BUILD_DEBUG)

install:
    @rem Not required

!include ..\Target.mak
