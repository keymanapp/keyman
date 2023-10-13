#
# Keyman64 Makefile
#

!include ..\..\Defines.mak

build: version.res dirs
    $(MKVER_U) exports.in exports.h
    $(MSBUILD) keyman64.vcxproj $(MSBUILD_BUILD) "/p:Platform=x64"
    $(COPY) $(X64_TARGET_PATH)\keyman64.dll $(PROGRAM)\engine
    $(COPY) $(X64_TARGET_PATH)\keyman64.pdb $(DEBUGPATH)\engine
#   $(COPY) $(X64_TARGET_PATH)\keyman64.lib $(OUTLIB) done by vcxproj

clean: def-clean
    $(MSBUILD) $(MSBUILD_CLEAN) keyman64.vcxproj "/p:Platform=x64"

signcode:
    $(SIGNCODE) /d "Keyman Engine" $(PROGRAM)\engine\keyman64.dll

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\engine\keyman64.dll /t keyman-engine-windows
    $(SYMSTORE) $(DEBUGPATH)\engine\keyman64.pdb /t keyman-engine-windows

install:
    $(COPY) $(PROGRAM)\engine\keyman64.dll "$(INSTALLPATH_KEYMANENGINE)\keyman64.dll"
    echo You may want to manually tweak keyman-debug-etw.man and fill in $(INSTALLPATH_KEYMANENGINE)\keyman64.dll
    echo and then run wevtutil im keyman-debug-etw.man to get the latest event tracing

!include ..\..\Target.mak
