#
# TODO: remove this project
#

!include ..\..\Defines.mak

build:
# version.res
    $(DCC32) km_yim.dpr
    rem $(TDSPACK) $(PROGRAM)\desktop\km_yim.exe km_yim.tds
    rem $(TDS2DBG) $(PROGRAM)\desktop\km_yim.exe
    $(WZZIP) inst_km_yim.zip km_yim.exe
# $(WZSE) inst_km_yim -setup -t inst_km_yim.dialog.txt -st "Tavultesoft Keyman Desktop Yahoo Messenger Addin" -c km_yim.exe

clean: def-clean
    if exist inst_km_yim.zip del inst_km_yim.zip

signcode:
    $(SIGNCODE) /d "Tavultesoft Keyman Desktop Yahoo Messenger Addin" inst_km_yim.exe

wrap-symbols:
    rem

!include ..\..\Target.mak
