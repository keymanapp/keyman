#
# KMCmpDll Makefile
#

!include ..\Defines.mak


build: version.res dirs
    $(MSBUILD) kmcmpdll.vcxproj $(MSBUILD_BUILD) "/p:Platform=Win32"
    $(COPY) $(WIN32_TARGET_PATH)\kmcmpdll.dll $(DEVELOPER_PROGRAM)
    $(COPY) $(WIN32_TARGET_PATH)\kmcmpdll.lib $(DEVELOPER_OUTLIB)
    $(COPY) $(WIN32_TARGET_PATH)\kmcmpdll.pdb $(DEVELOPER_DEBUGPATH)
# for debugging purposes, it's useful to have a copy
# of kmcmpdll.dll in the TIKE project folder; it is
# .gitignored
    -mkdir ..\kmcomp\$(WIN32_TARGET_PATH)
    -mkdir ..\tike\$(WIN32_TARGET_PATH)
    $(COPY) $(WIN32_TARGET_PATH)\kmcmpdll.dll ..\tike\$(WIN32_TARGET_PATH)\kmcmpdll.dll
    $(COPY) $(WIN32_TARGET_PATH)\kmcmpdll.dll ..\kmcomp\$(WIN32_TARGET_PATH)\kmcmpdll.dll

    $(MSBUILD) kmcmpdll.vcxproj $(MSBUILD_BUILD) "/p:Platform=x64"
    $(COPY) $(X64_TARGET_PATH)\kmcmpdll.x64.dll $(DEVELOPER_PROGRAM)\kmcmpdll.x64.dll
    $(COPY) $(X64_TARGET_PATH)\kmcmpdll.x64.lib $(DEVELOPER_OUTLIB)\kmcmpdll.x64.lib
    $(COPY) $(X64_TARGET_PATH)\kmcmpdll.x64.pdb $(DEVELOPER_DEBUGPATH)\kmcmpdll.x64.pdb
    -mkdir ..\kmcomp\$(X64_TARGET_PATH)
    $(COPY) $(X64_TARGET_PATH)\kmcmpdll.x64.dll ..\kmcomp\$(X64_TARGET_PATH)\kmcmpdll.x64.dll


clean: def-clean
    $(MSBUILD) kmcmpdll.vcxproj $(MSBUILD_CLEAN)

signcode:
    $(SIGNCODE) /d "Keyman Developer Compiler" $(DEVELOPER_PROGRAM)\kmcmpdll.dll
    $(SIGNCODE) /d "Keyman Developer Compiler" $(DEVELOPER_PROGRAM)\kmcmpdll.x64.dll

wrap-symbols:
    $(SYMSTORE) $(DEVELOPER_PROGRAM)\kmcmpdll.dll /t keyman-developer
    $(SYMSTORE) $(DEVELOPER_PROGRAM)\kmcmpdll.x64.dll /t keyman-developer
    $(SYMSTORE) $(DEVELOPER_DEBUGPATH)\kmcmpdll.pdb /t keyman-developer
    $(SYMSTORE) $(DEVELOPER_DEBUGPATH)\kmcmpdll.x64.pdb /t keyman-developer

test-manifest:
    @rem This target needed as dependency for TIKE and KMCMPDLL

install:
    $(COPY) $(DEVELOPER_PROGRAM)\kmcmpdll.dll "$(INSTALLPATH_KEYMANDEVELOPER)\kmcmpdll.dll"
    $(COPY) $(DEVELOPER_PROGRAM)\kmcmpdll.x64.dll "$(INSTALLPATH_KEYMANDEVELOPER)\kmcmpdll.x64.dll"

!include ..\Target.mak
