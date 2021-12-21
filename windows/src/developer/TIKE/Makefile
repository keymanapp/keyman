#
# TIKE Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res icons dirs xml xsd pull-core
    cd $(ROOT)\src\developer\tike
    $(DELPHI_MSBUILD) tike.dproj "/p:Platform=Win32"
    $(SENTRYTOOL_DELPHIPREP) $(WIN32_TARGET_PATH)\tike.exe -dpr tike.dpr
    $(TDS2DBG) $(WIN32_TARGET_PATH)\tike.exe
    $(COPY) $(WIN32_TARGET_PATH)\tike.exe $(PROGRAM)\developer
    $(COPY) kmlmc.cmd $(PROGRAM)\developer
    $(COPY) $(ROOT)\..\common\core\desktop\build\x86\$(TARGET_PATH)\src\kmnkbp0-0.dll $(PROGRAM)\developer
    if exist $(WIN32_TARGET_PATH)\tike.dbg $(COPY) $(WIN32_TARGET_PATH)\tike.dbg $(DEBUGPATH)\developer

xsd:
    $(COPY) $(KEYMAN_ROOT)\common\schemas\kps\kps.xsd $(PROGRAM)\developer

xml: pull-npm sentry-init.js
    cd $(ROOT)\src\developer\tike\xml
    -del /q $(ROOT)\bin\developer\xml\*
    xcopy /s /y * $(ROOT)\bin\developer\xml
    cd $(ROOT)\src\developer\tike

sentry-init.js:
    cd $(ROOT)\src\developer\tike\xml\app\lib\sentry
    $(MKVER_U) init.js.in init.js

pull-core:
    cd $(ROOT)\..\common\core\desktop
!ifdef GIT_BASH_FOR_KEYMAN
!ifdef DEBUG
    $(GIT_BASH_FOR_KEYMAN) build.sh --debug
!else
    $(GIT_BASH_FOR_KEYMAN) build.sh
!endif
!else
!ifdef DEBUG
    start /wait ./build.sh --debug
!else
    start /wait ./build.sh
!endif
!endif

pull-npm:
#
# Grab the version of Monaco and any other node modules
# that we want to use in Keyman Developer, and copy into
# lib folder for sane paths. If we use more npm modules in the
# future, we may consolidate the paths at that time.
#
    cd $(ROOT)\src\developer\tike\xml\app
    npm install
    -rd /s/q $(ROOT)\src\developer\tike\xml\app\lib\monaco
    xcopy /i /s /y $(ROOT)\src\developer\tike\xml\app\node_modules\monaco-editor\min $(ROOT)\src\developer\tike\xml\app\lib\monaco\min
    xcopy /i /s /y $(ROOT)\src\developer\tike\xml\app\node_modules\monaco-editor\min-maps $(ROOT)\src\developer\tike\xml\app\lib\monaco\min-maps

icons:
    rc icons.rc

clean: def-clean
    if exist icons.res del icons.res

signcode:
    $(SIGNCODE) /d "Keyman Developer" $(PROGRAM)\developer\tike.exe
    $(SIGNCODE) /d "Keyman Core" $(PROGRAM)\developer\kmnkbp0-0.dll

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\developer\tike.exe /t keyman-developer
    $(SYMSTORE) $(DEBUGPATH)\developer\tike.dbg /t keyman-developer

install:
    $(COPY) $(PROGRAM)\developer\tike.exe "$(INSTALLPATH_KEYMANDEVELOPER)\tike.exe"
    $(COPY) $(PROGRAM)\developer\kmnkbp0-0.dll "$(INSTALLPATH_KEYMANDEVELOPER)\kmnkbp0-0.dll"

test-manifest:
# test that linked manifest exists and correct
    $(MT) -nologo -inputresource:$(PROGRAM)\developer\tike.exe -validate_manifest

generate-app-wxs:
    @echo *******************************************************************************************
    @echo The generated ..\inst\monaco_gen.wxs file will require manual merging with the existing
    @echo monaco.xws. Existing component GUIDs must be maintained per Windows Installer component
    @echo rules (a file at a single location should have a permanent GUID for reference counting).
    @echo *******************************************************************************************
    $(WIXPATH)\heat.exe dir xml\app\lib\monaco\min\vs -gg -o ..\inst\monaco_gen.wxs -cg MonacoFiles -var var.MonacoSourceDir -dr MonacoTargetDir

!include ..\..\Target.mak

