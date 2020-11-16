#
# Keymanmc Makefile
#

!include ..\..\Defines.mak

build:
	mc -U keymanmc.mc
	rc keymanmc.rc
	link -dll -noentry keymanmc.res
    $(COPY) keymanmc.dll $(PROGRAM)\engine

clean: def-clean
    -del keymanmc.rc
    -del keymanmc.h
	-del keymanmc.dll
	-del keymanmc.res
	-del MSG00409.bin

signcode:
    $(SIGNCODE) /d "Keyman Engine" $(PROGRAM)\engine\keymanmc.dll

wrap-symbols:
		rem no symbols

backup:
    $(WZZIP) $(BUILD)\engine\keymanmc.zip  $(BACKUPDEFAULTS) keymanmc.dll

install:
    $(COPY) $(PROGRAM)\engine\keymanmc.dll "$(INSTALLPATH_KEYMANENGINE)\keymanmc.dll"
	reg add HKLM\SYSTEM\CurrentControlSet\services\eventlog\Application\Keyman /v CategoryCount /t REG_DWORD /d 1 /f
	reg add HKLM\SYSTEM\CurrentControlSet\services\eventlog\Application\Keyman /v CategoryMessageFile /t REG_SZ /d "$(INSTALLPATH_KEYMANENGINE)\keymanmc.dll" /f
	reg add HKLM\SYSTEM\CurrentControlSet\services\eventlog\Application\Keyman /v EventMessageFile /t REG_SZ /d "$(INSTALLPATH_KEYMANENGINE)\keymanmc.dll" /f
	reg add HKLM\SYSTEM\CurrentControlSet\services\eventlog\Application\Keyman /v TypesSupported /t REG_DWORD /d 7 /f

!include ..\..\Target.mak
