#
# TIKE Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res icons dirs xml
    cd $(ROOT)\src\developer\tike
    $(DCC32) tike.dpr
    $(TDS2DBG) tike.exe
    $(SENTRYTOOL_DELPHIPREP) tike.exe -dpr tike.dpr
    $(COPY) tike.exe $(PROGRAM)\developer
    $(COPY) kmlmc.cmd $(PROGRAM)\developer
    if exist tike.dbg $(COPY) tike.dbg $(DEBUGPATH)\developer

xml: pull-keymanweb pull-npm sentry-init.js
    cd $(ROOT)\src\developer\tike\xml
    -del /q $(ROOT)\bin\developer\xml\*
    xcopy /s /y * $(ROOT)\bin\developer\xml
    cd $(ROOT)\src\developer\tike

sentry-init.js:
    cd $(ROOT)\src\developer\tike\xml\app\lib\sentry
    $(MKVER_U) init.js.in init.js

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
    xcopy /s /y $(ROOT)\src\developer\tike\xml\app\node_modules\monaco-editor\min $(ROOT)\src\developer\tike\xml\app\lib\monaco\min\ #
    xcopy /s /y $(ROOT)\src\developer\tike\xml\app\node_modules\monaco-editor\min-maps $(ROOT)\src\developer\tike\xml\app\lib\monaco\min-maps\ #

pull-keymanweb:
    #
    # Requires us to build KMW in order to get the output files
    # If GIT_BASH_FOR_KEYMAN variable is not defined, then will just throw up
    # a new window to do the build. This means we lose logging and
    # potentially exit values as well. So GIT_BASH should be set for
    # build agents, etc, to something like
    #
    # GIT_BASH_FOR_KEYMAN="C:\Program Files\Git\bin\bash.exe" --init-file "c:\Program Files\Git\etc\profile" -l
    #
    cd $(ROOT)\..\web\source

!ifdef GIT_BASH_FOR_KEYMAN
    $(GIT_BASH_FOR_KEYMAN) build.sh -no_minify
!else
    start /wait ./build.sh -no_minify
!endif

    -rd /s/q $(ROOT)\src\developer\tike\xml\kmw\resource
    mkdir $(ROOT)\src\developer\tike\xml\kmw\resource
    xcopy /y $(ROOT)\..\web\release\unminified\web\*.js $(ROOT)\src\developer\tike\xml\kmw\resource\ #
    xcopy /y $(ROOT)\..\web\release\unminified\web\*.js.map $(ROOT)\src\developer\tike\xml\kmw\resource\ #
    xcopy /y $(ROOT)\..\web\LICENSE $(ROOT)\src\developer\tike\xml\kmw\resource\ #
    xcopy /y $(ROOT)\..\web\README.md $(ROOT)\src\developer\tike\xml\kmw\resource\ #
    xcopy /s /y $(ROOT)\..\web\release\unminified\web\osk\* $(ROOT)\src\developer\tike\xml\kmw\resource\osk\ #
    xcopy /s /y $(ROOT)\..\web\release\unminified\web\ui\* $(ROOT)\src\developer\tike\xml\kmw\resource\ui\ #

icons:
    rc icons.rc

clean: def-clean
    if exist icons.res del icons.res

signcode:
    $(SIGNCODE) /d "Keyman Developer" $(PROGRAM)\developer\tike.exe

backup:
    $(WZZIP) $(BUILD)\developer\tike.zip  $(BACKUPDEFAULTS) tike.exe


install:
    $(COPY) $(PROGRAM)\developer\tike.exe "$(INSTALLPATH_KEYMANDEVELOPER)\tike.exe"

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

