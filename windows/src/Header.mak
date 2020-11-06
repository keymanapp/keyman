#
# Header.mak - used for makefiles which are in parent folders
#

HEADER_MAK=1

!IFNDEF TARGETS
!ERROR You must define the targets before including the Header.mak file!
!ENDIF

!IFDEF KEYMAN_ROOT
ROOT=$(KEYMAN_ROOT)\windows
!ELSE
ROOT=c:\keyman\windows
!ENDIF

# This path will need to be updated if the root path changes

!include $(ROOT)\src\Defines.mak

#
# The targets build, signcode, symbols, backup are standard
# targets for all projects
#

build: $(BUILDPREREQ)
    $(MAKE) -DTARGET=build $(TARGETS)

!IFNDEF NOTARGET_SIGNCODE
signcode:
    $(MAKE) -DTARGET=signcode $(TARGETS)

symbols:
    $(MAKE) -DTARGET=symbols $(TARGETS)

backup:
    $(MAKE) -DTARGET=backup $(TARGETS)
!ELSE
signcode:
    rem no signcode required

symbols:
    rem no symbols required

backup:
    rem no backup required
!ENDIF

build-release:
!IFDEF RELEASE_TARGETS
    $(MAKE) -DTARGET=build-release $(RELEASE_TARGETS)
!ELSE
    @rem
!ENDIF

clean:
    $(MAKE) -DTARGET=clean $(TARGETS) $(CLEANS)

install:
    $(MAKE) -DTARGET=install $(TARGETS)

test-manifest:
!IFDEF MANIFESTS
    $(MAKE) -DTARGET=test-manifest $(MANIFESTS)
!ELSE
    $(MAKE) -DTARGET=test-manifest $(TARGETS)
!ENDIF