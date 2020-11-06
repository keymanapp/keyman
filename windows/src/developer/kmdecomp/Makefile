#
# kmdecomp Makefile
#

!include ..\..\Defines.mak


build: version.res dirs
    $(MSBUILD) kmdecomp.sln $(MSBUILD_BUILD) /p:Platform=Win32
    $(COPY) $(WIN32_TARGET_PATH)\kmdecomp.exe $(PROGRAM)\developer
    $(COPY) $(WIN32_TARGET_PATH)\kmdecomp.pdb $(DEBUGPATH)\developer

clean: def-clean
    $(MSBUILD) kmdecomp.sln $(MSBUILD_CLEAN)

signcode:
    $(SIGNCODE) /d "Keyman Developer Decompiler" $(PROGRAM)\developer\kmdecomp.exe

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\developer\kmdecomp.exe /t keyman-developer
    $(SYMSTORE) $(DEBUGPATH)\developer\kmdecomp.pdb /t keyman-developer

backup:
    $(WZZIP) $(BUILD)\developer\kmdecomp.zip  $(BACKUPDEFAULTS) $(PROGRAM)\developer\kmdecomp.exe

test-manifest:
    @rem This target needed as dependency for KMDECOMP

install:
    $(COPY) $(PROGRAM)\developer\kmdecomp.exe "$(INSTALLPATH_KEYMANDEVELOPER)\kmdecomp.exe"

!include ..\..\Target.mak
