# Copy files to correct names in \release; this file is munged by mkver to download.mak
#

!include ..\..\defines.mak

default:
  echo Please don't call this file directly -- used by Makefile

clean:
    $(MSBUILD) keyman64-ver.sln $(MSBUILD_CLEAN) "/p:Platform=x64;VersionWithTag=17.0.48-alpha-local"

signcode:
    $(SIGNCODE) /d "Keyman Engine x64" $(PROGRAM)\engine\keyman64-ver17.0.48-alpha-local.dll

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\engine\keyman64-ver17.0.48-alpha-local.dll /t keyman-engine-windows
    $(SYMSTORE) $(DEBUGPATH)\engine\keyman64-ver17.0.48-alpha-local.pdb /t keyman-engine-windows

install:
    $(COPY) $(PROGRAM)\engine\keyman64-ver17.0.48-alpha-local.dll "$(INSTALLPATH_KEYMANENGINE)\keyman64-ver17.0.48-alpha-local.dll"


build:
    $(MSBUILD) keyman64-ver.sln $(MSBUILD_BUILD) "/p:Platform=x64;VersionWithTag=17.0.48-alpha-local"
    $(COPY) $(X64_TARGET_PATH)\keyman64-ver17.0.48-alpha-local.dll $(PROGRAM)\engine
    $(COPY) $(X64_TARGET_PATH)\keyman64-ver17.0.48-alpha-local.pdb $(DEBUGPATH)\engine
