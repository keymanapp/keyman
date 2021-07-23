#
# Buildpkg Makefile
#

!include ..\..\Defines.mak

build: version.res dirs
    $(DELPHI_MSBUILD) buildpkg.dproj /p:Platform=Win32
    $(SENTRYTOOL_DELPHIPREP) $(WIN32_TARGET_PATH)\buildpkg.exe -dpr buildpkg.dpr
    $(TDS2DBG) $(WIN32_TARGET_PATH)\buildpkg.exe
    $(COPY) $(WIN32_TARGET_PATH)\Buildpkg.exe $(PROGRAM)\online
    $(COPY) $(WIN32_TARGET_PATH)\Buildpkg.exe $(PROGRAM)\buildtools
    if exist $(WIN32_TARGET_PATH)\buildpkg.dbg $(COPY) $(WIN32_TARGET_PATH)\buildpkg.dbg $(DEBUGPATH)\buildtools

clean: def-clean

signcode:
    $(SIGNCODE) /d "Package Installer Creator for Server" $(PROGRAM)\online\Buildpkg.exe

!include ..\..\Target.mak
