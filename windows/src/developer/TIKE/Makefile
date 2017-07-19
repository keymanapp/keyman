#
# TIKE Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res icons dirs xml
    cd $(ROOT)\src\developer\tike
    $(DCC32) tike.dpr
    $(TDS2DBG) tike.exe 
    $(MAKEJCLDBG) tike.map
    $(COPY) tike.exe $(PROGRAM)\developer
    if exist tike.dbg $(COPY) tike.dbg $(DEBUGPATH)\developer

xml: pull-keymanweb 
    cd $(ROOT)\src\developer\tike\xml
    -del /q $(ROOT)\bin\developer\xml\*
    xcopy /s /y * $(ROOT)\bin\developer\xml
    cd $(ROOT)\src\developer\tike

pull-keymanweb:
    -rd /s/q $(ROOT)\src\developer\tike\xml\kmw\resource
    mkdir $(ROOT)\src\developer\tike\xml\kmw\resource
    xcopy /y $(ROOT)\..\web\source\*.js $(ROOT)\src\developer\tike\xml\kmw\resource\ #
    xcopy /s /y $(ROOT)\..\web\source\resources\* $(ROOT)\src\developer\tike\xml\kmw\resource\ #

icons:
    rc icons.rc

clean: def-clean
    if exist icons.res del icons.res

signcode:
    $(SIGNCODE) /d "Keyman Developer" $(PROGRAM)\developer\tike.exe

backup:
    $(WZZIP) $(BUILD)\developer\tike.zip  $(BACKUPDEFAULTS) tike.exe


install:
    $(COPY) $(PROGRAM)\developer\tike.exe "c:\Program Files (x86)\Tavultesoft\Keyman Developer 9.0\tike.exe"

test-manifest:
    # test that linked manifest exists and correct
    $(MT) -nologo -inputresource:$(PROGRAM)\developer\tike.exe -validate_manifest

!include ..\..\Target.mak

