#
# Devtools Makefile
#

#
# Devtools is used to create PathDefines.mak, so
# it may not be present when we build. So as we
# don't need it for the build, let's go ahead and
# delete it (Devtools has all dependencies
# referenced explictly to avoid chicken-and-egg
# here).
#
EXCLUDEPATHDEFINES=1

!include ..\..\Defines.mak

build:
    $(DELPHI_MSBUILD) devtools.dproj

test-releasebuildcheck: build
    $(DEVTOOLS) -rt

clean: def-clean

!include ..\..\Target.mak
