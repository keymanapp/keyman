#
# kmdecomp Makefile
#

!include ..\Defines.mak


build: version.res dirs
    $(MSBUILD) kmdecomp.sln $(MSBUILD_BUILD) "/p:Platform=Win32"
    $(COPY) $(WIN32_TARGET_PATH)\kmdecomp.exe $(DEVELOPER_PROGRAM)
    $(COPY) $(WIN32_TARGET_PATH)\kmdecomp.pdb $(DEVELOPER_DEBUGPATH)

clean: def-clean
    $(MSBUILD) kmdecomp.sln $(MSBUILD_CLEAN)

signcode:
    $(SIGNCODE) /d "Keyman Developer Decompiler" $(DEVELOPER_PROGRAM)\kmdecomp.exe

wrap-symbols:
    $(SYMSTORE) $(DEVELOPER_PROGRAM)\kmdecomp.exe /t keyman-developer
    $(SYMSTORE) $(DEVELOPER_DEBUGPATH)\kmdecomp.pdb /t keyman-developer

test-manifest:
    @rem This target needed as dependency for KMDECOMP

install:
    $(COPY) $(DEVELOPER_PROGRAM)\kmdecomp.exe "$(INSTALLPATH_KEYMANDEVELOPER)\kmdecomp.exe"

!include ..\Target.mak
