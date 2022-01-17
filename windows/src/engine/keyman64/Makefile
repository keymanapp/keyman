#
# Keyman64 Makefile
#

!include ..\..\Defines.mak

build: version.res dirs
    $(MSBUILD) keyman64.sln $(MSBUILD_BUILD) "/p:Platform=x64"
    $(COPY) $(X64_TARGET_PATH)\keyman64.dll $(PROGRAM)\engine
    $(COPY) $(X64_TARGET_PATH)\keyman64.pdb $(DEBUGPATH)\engine
#   $(COPY) keyman64.lib $(OUTLIB) - already done

clean: def-clean
    $(MSBUILD) keyman64.sln $(MSBUILD_CLEAN) "/p:Platform=x64"

signcode:
    $(SIGNCODE) /d "Keyman Engine x64" $(PROGRAM)\engine\keyman64.dll

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\engine\keyman64.dll /t keyman-engine-windows
    $(SYMSTORE) $(DEBUGPATH)\engine\keyman64.pdb /t keyman-engine-windows

install:
    $(COPY) $(PROGRAM)\engine\keyman64.dll "$(INSTALLPATH_KEYMANENGINE)\keyman64.dll"

!include ..\..\Target.mak
