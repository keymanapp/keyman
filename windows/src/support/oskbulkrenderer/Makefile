#
# oskbulkrenderer Makefile
#

!include ..\..\Defines.mak

build: version.res dirs
    $(DELPHI_MSBUILD) oskbulkrenderer.dproj /p:Platform=Win32

    $(SENTRYTOOL_DELPHIPREP) $(WIN32_TARGET_PATH)\oskbulkrenderer.exe -dpr oskbulkrenderer.dpr
    $(TDS2DBG) $(WIN32_TARGET_PATH)\oskbulkrenderer.exe
    $(COPY) $(WIN32_TARGET_PATH)\oskbulkrenderer.exe $(PROGRAM)\support
    if exist $(WIN32_TARGET_PATH)\oskbulkrenderer.dbg $(COPY) $(WIN32_TARGET_PATH)\oskbulkrenderer.dbg $(DEBUGPATH)\support

clean: def-clean
    -rd /s/q Win32

signcode:
    rem Not signing this utility

backup:
    rem No source code backup needed

wrap-symbols:
    @rem

!include ..\..\Target.mak
