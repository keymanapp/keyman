#
# etl2log Makefile
#

!include ..\..\Defines.mak

build: version.res dirs
    $(MSBUILD) etl2log.vcxproj $(MSBUILD_BUILD)
    $(COPY) $(WIN32_TARGET_PATH)\etl2log.exe $(PROGRAM)\support
    $(COPY) $(WIN32_TARGET_PATH)\etl2log.pdb $(DEBUGPATH)\support

clean: def-clean
    $(MSBUILD) etl2log.vcxproj $(MSBUILD_CLEAN)

signcode:
    $(SIGNCODE) /d "Keyman Engine Tools" $(PROGRAM)\support\etl2log.exe

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\support\etl2log.exe /t keyman-windows
    $(SYMSTORE) $(DEBUGPATH)\support\etl2log.pdb /t keyman-windows

!include ..\..\Target.mak
