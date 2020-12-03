#-------------------------------------------------------
# Makefile for building help file
#-------------------------------------------------------

!include ..\..\Defines.mak

#-------------------------------------------------------
# Main targets
#-------------------------------------------------------

build:
    @rem

build-release: chm

install: web

chm: dirs html

html:
!ifdef GIT_BASH_FOR_KEYMAN
    $(GIT_BASH_FOR_KEYMAN) build.sh -html
!else
    start /wait ./build.sh -html
!endif

web:
!ifdef GIT_BASH_FOR_KEYMAN
    $(GIT_BASH_FOR_KEYMAN) build.sh -web
!else
    start /wait ./build.sh -web
!endif

signcode:
    @rem

backup:
    @rem

clean:
    @rem

!include ..\..\Target.mak
