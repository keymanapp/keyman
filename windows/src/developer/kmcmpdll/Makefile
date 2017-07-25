#
# KMCmpDll Makefile
#

!include ..\..\Defines.mak


build: version.res dirs
    $(MSBUILD) kmcmpdll.sln $(MSBUILD_BUILD)
    $(COPY) kmcmpdll.dll $(PROGRAM)\developer
    $(COPY) kmcmpdll.lib $(OUTLIB)
    $(COPY) kmcmpdll.pdb $(DEBUGPATH)\developer

clean: def-clean
    $(MSBUILD) kmcmpdll.sln $(MSBUILD_CLEAN)

signcode:
    $(SIGNCODE) /d "Keyman Developer Compiler" $(PROGRAM)\developer\kmcmpdll.dll

backup:
    $(WZZIP) $(BUILD)\developer\kmcmpdll.zip  $(BACKUPDEFAULTS) kmcmpdll.dll

test-manifest:
    @rem This target needed as dependency for TIKE and KMCMPDLL

install:
    $(COPY) $(PROGRAM)\developer\kmcmpdll.dll "c:\program files (x86)\tavultesoft\keyman developer 9.0\kmcmpdll.dll"
    
!include ..\..\Target.mak
