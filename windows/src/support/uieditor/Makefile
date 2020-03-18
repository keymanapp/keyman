#
# UIEditor Makefile
#

!include ..\..\Defines.mak

build: version.res dirs
    $(DCC32) UIEditor.dpr
    $(TDS2DBG) UIEditor.exe
    $(SENTRYTOOL_DELPHIPREP) UIEditor.exe -dpr UIEditor.dpr
    $(COPY) UIEditor.exe $(PROGRAM)\support

signcode:
    $(SIGNCODE) /d "Tavultesoft Keyman Desktop UI Editor" $(PROGRAM)\support\UIEditor.exe

backup:
    $(WZZIP) $(BUILD)\desktop\UIEditor.zip  $(BACKUPDEFAULTS) UIEditor.exe

!include ..\..\Target.mak
