#
# GetFileLocks Makefile
#

!include ..\..\Defines.mak

!ifdef DEBUG
    NMCFG=getfilelocks - Win32 Debug
!else
    NMCFG=getfilelocks - Win32 Release
!endif

build: dirs version.res
    $(MSBUILD) getfilelocks.sln $(MSBUILD_BUILD)
    $(COPY) getfilelocks.exe $(PROGRAM)\buildtools

clean: def-clean
    $(MSBUILD) getfilelocks.sln $(MSBUILD_CLEAN)

!include ..\..\Target.mak
