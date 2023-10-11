#
# KDebug Makefile
#

!include ..\..\Defines.mak

!ifdef DEBUG
    NMCFG=kdebug - Win32 Debug
!else
    NMCFG=kdebug - Win32 Release
!endif

install: dirs build

build: version.res
    $(NMAKE) /f kdebug.mak "CFG=$(NMCFG)"

clean: def-clean
    $(NMAKE) /f kdebug.mak "CFG=$(NMCFG)" CLEAN

!include ..\..\Target.mak
