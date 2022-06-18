#
# KMAnalyze Makefile
#

!include ..\..\Defines.mak

build: version.res dirs
    $(MSBUILD) kmanalyze.vcxproj $(MSBUILD_BUILD)
    $(COPY) $(WIN32_TARGET_PATH)\kmanalyze.exe $(PROGRAM)\developer
    $(COPY) $(WIN32_TARGET_PATH)\kmanalyze.pdb $(DEBUGPATH)\developer

clean: def-clean
    $(MSBUILD) kmanalyze.sln $(MSBUILD_CLEAN)

signcode:
    $(SIGNCODE) /d "Keyman Developer Keyboard Analyzer" $(PROGRAM)\developer\kmanalyze.exe

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\developer\kmanalyze.exe /t keyman-developer
    $(SYMSTORE) $(DEBUGPATH)\developer\kmanalyze.pdb /t keyman-developer

test-manifest:
    @rem

install:
    $(COPY) $(PROGRAM)\developer\kmanalyze.exe "$(INSTALLPATH_KEYMANDEVELOPER)\kmanalyze.exe"

!include ..\..\Target.mak
