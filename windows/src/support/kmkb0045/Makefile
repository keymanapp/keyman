#
# TODO: remove this project
#

!include ..\..\Defines.mak

build: version.res manifest.res
    $(DCC32) kmkb0045.dpr
    $(SENTRYTOOL_DELPHIPREP) kmkb0045.exe -dpr kmkb0045.dpr
    $(TDS2DBG) kmkb0045.exe
    $(COPY) kmkb0045.exe $(PROGRAM)\support

clean: def-clean

signcode:
    $(SIGNCODE) /d "Tavultesoft KMKB0045" $(PROGRAM)\support\kmkb0045.exe

backup:
    $(WZZIP) $(BUILD)\support\kmkb0045.zip  $(BACKUPDEFAULTS) kmkb0045.exe


!include ..\..\Target.mak
