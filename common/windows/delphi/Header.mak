#
# Header.mak - used for makefiles which are in parent folders
#

HEADER_MAK=1

!IFNDEF TARGETS
!ERROR You must define the targets before including the Header.mak file!
!ENDIF

!IFNDEF KEYMAN_ROOT
!ERROR KEYMAN_ROOT must be defined!
!ENDIF

# This path will need to be updated if the root path changes

!include $(KEYMAN_ROOT)\common\windows\delphi\Defines.mak

#
# The targets build, signcode, symbols are standard
# targets for all projects
#

build: $(BUILDPREREQ)
    $(MAKE) "TARGET=build" $(TARGETS)

!IFNDEF NOTARGET_SIGNCODE
signcode:
    $(MAKE) "TARGET=signcode" $(TARGETS)

symbols:
    $(MAKE) "TARGET=symbols" $(TARGETS)
!ELSE
signcode:
    rem no signcode required

symbols:
    rem no symbols required
!ENDIF

build-release:
!IFDEF RELEASE_TARGETS
    $(MAKE) "TARGET=build-release" $(RELEASE_TARGETS)
!ELSE
    @rem
!ENDIF

clean:
    $(MAKE) "TARGET=clean" $(TARGETS) $(CLEANS)

install:
    $(MAKE) "TARGET=install" $(TARGETS)

test-manifest:
!IFDEF MANIFESTS
    $(MAKE) "TARGET=test-manifest" $(MANIFESTS)
!ELSE
    $(MAKE) "TARGET=test-manifest" $(TARGETS)
!ENDIF