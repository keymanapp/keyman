#
# imsample Makefile
#

!include ..\..\Defines.mak

!ifdef DEBUG
    NMCFG=IMSample - Win32 Debug
!else
    NMCFG=IMSample - Win32 Release
!endif

install: dirs build
    $(COPY) imsample.dll $(PROGRAM)\dev

build: version.res
    $(NMAKE) /f imsample.mak "CFG=$(NMCFG)"

clean: def-clean
    $(NMAKE) /f imsample.mak "CFG=$(NMCFG)" CLEAN

!include ..\..\Target.mak
