#
# Kmconvert Makefile
#

!include ..\Defines.mak

build: version.res manifest.res dirs icons
    $(DELPHI_MSBUILD) kmconvert.dproj "/p:Platform=Win32"

    $(SENTRYTOOL_DELPHIPREP) $(WIN32_TARGET_PATH)\kmconvert.exe -dpr kmconvert.dpr
    $(TDS2DBG) $(WIN32_TARGET_PATH)\kmconvert.exe
    $(COPY) $(WIN32_TARGET_PATH)\kmconvert.exe $(DEVELOPER_PROGRAM)
    if exist $(WIN32_TARGET_PATH)\kmconvert.dbg $(COPY) $(WIN32_TARGET_PATH)\kmconvert.dbg $(DEVELOPER_DEBUGPATH)

# Copy template files to project
    -rd /s/q $(DEVELOPER_PROGRAM)\projects\templates
    -mkdir $(DEVELOPER_PROGRAM)\projects\templates
    xcopy /s /y data\* $(DEVELOPER_PROGRAM)\projects\templates

icons:
    rc icons.rc

clean: def-clean
    if exist icons.res del icons.res

signcode:
    $(SIGNCODE) /d "Keyman Developer Conversion Utility" $(DEVELOPER_PROGRAM)\kmconvert.exe

wrap-symbols:
    $(SYMSTORE) $(DEVELOPER_PROGRAM)\kmconvert.exe /t keyman-developer
    $(SYMSTORE) $(DEVELOPER_DEBUGPATH)\kmconvert.dbg /t keyman-developer

test-manifest:
# test that (a) linked manifest exists and correct
    $(MT) -nologo -inputresource:$(DEVELOPER_PROGRAM)\kmconvert.exe -validate_manifest

install:
    $(COPY) $(DEVELOPER_PROGRAM)\kmconvert.exe "$(INSTALLPATH_KEYMANDEVELOPER)\kmconvert.exe"

!include ..\Target.mak
