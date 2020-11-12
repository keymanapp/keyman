#
# KMAnalyze Makefile
#

!include ..\..\Defines.mak

build: version.res dirs
    $(MSBUILD) kmanalyze.sln $(MSBUILD_BUILD)
    $(COPY) $(TARGET_PATH)\kmanalyze.exe $(PROGRAM)\developer
    $(COPY) $(TARGET_PATH)\kmanalyze.pdb $(DEBUGPATH)\developer

clean: def-clean
    $(MSBUILD) kmanalyze.sln $(MSBUILD_CLEAN)

signcode:
    $(SIGNCODE) /d "Keyman Developer Keyboard Analyzer" $(PROGRAM)\developer\kmanalyze.exe

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\developer\kmanalyze.exe /t keyman-developer
    $(SYMSTORE) $(DEBUGPATH)\developer\kmanalyze.pdb /t keyman-developer

backup:
    $(WZZIP) $(BUILD)\developer\kmanalyze.exe $(BACKUPDEFAULTS) kmanalyze.exe

test-manifest:
    @rem

#install:
#    $(COPY) $(PROGRAM)\developer\kmanalyze.exe "$(INSTALLPATH_KEYMANDEVELOPER)\kmanalyze.exe"

!include ..\..\Target.mak
