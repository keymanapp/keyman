#
# test_httpuploader Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res
    $(DCC32) test_httpuploader.dpr

icons:
    rc icons.rc

clean: def-clean

signcode:
    rem no signcode

!include ..\..\Target.mak
