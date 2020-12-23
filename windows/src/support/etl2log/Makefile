#
# etl2log Makefile
#

!include ..\..\Defines.mak

build: version.res dirs
    $(MSBUILD) etl2log.vcxproj $(MSBUILD_BUILD)
    $(COPY) $(MSBUILD_TARGET)\etl2log.exe $(PROGRAM)\support
    $(COPY) $(MSBUILD_TARGET)\etl2log.pdb $(DEBUGPATH)\support

clean: def-clean
    $(MSBUILD) etl2log.vcxproj $(MSBUILD_CLEAN)

signcode:
    $(SIGNCODE) /d "Keyman Engine Tools" $(PROGRAM)\support\etl2log.exe

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\support\etl2log.exe /t keyman-windows
    $(SYMSTORE) $(DEBUGPATH)\support\etl2log.pdb /t keyman-windows

backup:
    $(WZZIP) $(BUILD)\support\etl2log.zip  $(BACKUPDEFAULTS) etl2log.exe

!include ..\..\Target.mak
