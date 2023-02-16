#
# Keyman64-ver Makefile
#

!include ..\..\Defines.mak

build: version.res dirs
    $(MSBUILD) keyman64-ver.sln $(MSBUILD_BUILD) "/p:Platform=x64;VersionWithTag=$(__VERSION_WITH_TAG)"
    $(COPY) $(X64_TARGET_PATH)\keyman64-ver$(__VERSION_WITH_TAG).dll $(PROGRAM)\engine
    $(COPY) $(X64_TARGET_PATH)\keyman64-ver$(__VERSION_WITH_TAG).pdb $(DEBUGPATH)\engine
#   $(COPY) keyman64-ver.lib $(OUTLIB) - already done

clean: def-clean
    $(MSBUILD) keyman64-ver.sln $(MSBUILD_CLEAN) "/p:Platform=x64;VersionWithTag=$(__VERSION_WITH_TAG)"

signcode:
    $(SIGNCODE) /d "Keyman Engine x64" $(PROGRAM)\engine\keyman64-ver$(__VERSION_WITH_TAG).dll

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\engine\keyman64-ver$(__VERSION_WITH_TAG).dll /t keyman-engine-windows
    $(SYMSTORE) $(DEBUGPATH)\engine\keyman64-ver$(__VERSION_WITH_TAG).pdb /t keyman-engine-windows

install:
    $(COPY) $(PROGRAM)\engine\keyman64-ver$(__VERSION_WITH_TAG).dll "$(INSTALLPATH_KEYMANENGINE)\keyman64-ver$(__VERSION_WITH_TAG).dll"

!include ..\..\Target.mak
