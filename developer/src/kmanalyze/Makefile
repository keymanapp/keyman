#
# KMAnalyze Makefile
#

!include ..\Defines.mak

build: version.res dirs
    $(MSBUILD) kmanalyze.vcxproj $(MSBUILD_BUILD)
    $(COPY) $(WIN32_TARGET_PATH)\kmanalyze.exe $(DEVELOPER_PROGRAM)
    $(COPY) $(WIN32_TARGET_PATH)\kmanalyze.pdb $(DEVELOPER_DEBUGPATH)

clean: def-clean
    $(MSBUILD) kmanalyze.sln $(MSBUILD_CLEAN)

signcode:
    $(SIGNCODE) /d "Keyman Developer Keyboard Analyzer" $(DEVELOPER_PROGRAM)\kmanalyze.exe

wrap-symbols:
    $(SYMSTORE) $(DEVELOPER_PROGRAM)\kmanalyze.exe /t keyman-developer
    $(SYMSTORE) $(DEVELOPER_DEBUGPATH)\kmanalyze.pdb /t keyman-developer

test-manifest:
    @rem

install:
    $(COPY) $(DEVELOPER_PROGRAM)\kmanalyze.exe "$(INSTALLPATH_KEYMANDEVELOPER)\kmanalyze.exe"

!include ..\Target.mak
