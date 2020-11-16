#
# Buildpkg Makefile
#

!include ..\..\Defines.mak

build: version.res dirs
    $(DCC32) Buildpkg.dpr
    $(SENTRYTOOL_DELPHIPREP) buildpkg.exe -dpr buildpkg.dpr
    $(TDS2DBG) buildpkg.exe
    $(COPY) Buildpkg.exe $(PROGRAM)\online
    $(COPY) Buildpkg.exe $(PROGRAM)\buildtools
    if exist buildpkg.dbg $(COPY) buildpkg.dbg $(DEBUGPATH)\buildtools

clean: def-clean

signcode:
    $(SIGNCODE) /d "Package Installer Creator for Server" $(PROGRAM)\online\Buildpkg.exe

backup:
    rem #$(WZZIP) $(BUILD)\buildtools\Buildpkg.zip $(BACKUPDEFAULTS) Buildpkg.exe

!include ..\..\Target.mak
