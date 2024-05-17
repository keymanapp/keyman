#
# TIKE Makefile
#

!include ..\Defines.mak

build: version.res manifest.res icons dirs xml xsd pull-core
    cd $(DEVELOPER_ROOT)\src\tike
		$(COPY) $(KEYMAN_ROOT)\common\resources\fonts\keymanweb-osk.ttf $(DEVELOPER_ROOT)\src\tike\xml\layoutbuilder\keymanweb-osk.ttf
    $(DELPHI_MSBUILD) tike.dproj "/p:Platform=Win32"
    $(SENTRYTOOL_DELPHIPREP) $(WIN32_TARGET_PATH)\tike.exe -dpr tike.dpr
    $(TDS2DBG) $(WIN32_TARGET_PATH)\tike.exe
    $(COPY) $(WIN32_TARGET_PATH)\tike.exe $(DEVELOPER_PROGRAM)
    $(COPY) kmlmc.cmd $(DEVELOPER_PROGRAM)
    $(COPY) kmlmp.cmd $(DEVELOPER_PROGRAM)
    $(COPY) kmc.cmd $(DEVELOPER_PROGRAM)
    $(COPY) $(KEYMAN_ROOT)\core\build\x86\$(TARGET_PATH)\src\$(KEYMANCORE).dll $(DEVELOPER_PROGRAM)
    if exist $(WIN32_TARGET_PATH)\tike.dbg $(COPY) $(WIN32_TARGET_PATH)\tike.dbg $(DEVELOPER_DEBUGPATH)
    $(COPY) $(KEYMAN_ROOT)\core\build\x86\$(TARGET_PATH)\src\$(KEYMANCORE).dll $(WIN32_TARGET_PATH)
    $(COPY) $(KEYMAN_ROOT)\core\build\x86\$(TARGET_PATH)\src\$(KEYMANCORE).pdb $(WIN32_TARGET_PATH)

xsd:
    $(COPY) $(KEYMAN_ROOT)\common\schemas\kps\kps.xsd $(DEVELOPER_PROGRAM)

xml: pull-npm sentry-init.js
    cd $(DEVELOPER_ROOT)\src\tike\xml
    -del /q $(DEVELOPER_PROGRAM)\xml\*
    xcopy /s /y * $(DEVELOPER_PROGRAM)\xml
    cd $(DEVELOPER_ROOT)\src\tike

sentry-init.js:
    cd $(DEVELOPER_ROOT)\src\tike\xml\app\lib\sentry
    $(MKVER_U) init.js.in init.js

pull-core:
    cd $(KEYMAN_ROOT)\core
!ifdef GIT_BASH_FOR_KEYMAN
!ifdef DEBUG
    $(GIT_BASH_FOR_KEYMAN) build.sh --no-tests clean:x86 configure:x86 build:x86 --debug
!else
    $(GIT_BASH_FOR_KEYMAN) build.sh --no-tests clean:x86 configure:x86 build:x86
!endif
!else
!ifdef DEBUG
    start /wait ./build.sh --no-tests clean:x86 configure:x86 build:x86 --debug
!else
    start /wait ./build.sh --no-tests clean:x86 configure:x86 build:x86
!endif
!endif

pull-npm:
#
# Grab the version of Monaco and any other node modules
# that we want to use in Keyman Developer, and copy into
# lib folder for sane paths. If we use more npm modules in the
# future, we may consolidate the paths at that time.
#
    cd $(DEVELOPER_ROOT)\src\tike\xml\app
    npm install
    -rd /s/q $(DEVELOPER_ROOT)\src\tike\xml\app\lib\monaco
    xcopy /i /s /y $(DEVELOPER_ROOT)\src\tike\xml\app\node_modules\monaco-editor\min $(DEVELOPER_ROOT)\src\tike\xml\app\lib\monaco\min
    xcopy /i /s /y $(DEVELOPER_ROOT)\src\tike\xml\app\node_modules\monaco-editor\min-maps $(DEVELOPER_ROOT)\src\tike\xml\app\lib\monaco\min-maps

icons:
    rc icons.rc

clean: def-clean
    if exist icons.res del icons.res

signcode:
    $(SIGNCODE) /d "Keyman Developer" $(DEVELOPER_PROGRAM)\tike.exe
    $(SIGNCODE) /d "Keyman Core" $(DEVELOPER_PROGRAM)\$(KEYMANCORE).dll
# Sign the Sentry executables and libraries here
    $(SIGNCODE) /d "Keyman Developer" $(DEVELOPER_PROGRAM)\sentry.dll
    $(SIGNCODE) /d "Keyman Developer" $(DEVELOPER_PROGRAM)\sentry.x64.dll
    $(SIGNCODE) /d "Keyman Developer" $(DEVELOPER_PROGRAM)\crashpad_handler.exe

wrap-symbols:
    $(SYMSTORE) $(DEVELOPER_PROGRAM)\tike.exe /t keyman-developer
    $(SYMSTORE) $(DEVELOPER_DEBUGPATH)\tike.dbg /t keyman-developer

install:
    $(COPY) $(DEVELOPER_PROGRAM)\tike.exe "$(INSTALLPATH_KEYMANDEVELOPER)\tike.exe"
    $(COPY) $(DEVELOPER_PROGRAM)\$(KEYMANCORE).dll "$(INSTALLPATH_KEYMANDEVELOPER)\$(KEYMANCORE).dll"

test-manifest:
# test that linked manifest exists and correct
    $(MT) -nologo -inputresource:$(DEVELOPER_PROGRAM)\tike.exe -validate_manifest

generate-app-wxs:
    @echo *******************************************************************************************
    @echo The generated ..\inst\monaco_gen.wxs file will require manual merging with the existing
    @echo monaco.xws. Existing component GUIDs must be maintained per Windows Installer component
    @echo rules (a file at a single location should have a permanent GUID for reference counting).
    @echo *******************************************************************************************
    $(WIXPATH)\heat.exe dir xml\app\lib\monaco\min\vs -gg -o ..\inst\monaco_gen.wxs -cg MonacoFiles -var var.MonacoSourceDir -dr MonacoTargetDir

!include ..\Target.mak

