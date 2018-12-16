#
# oskbulkrenderer Makefile
#

!include ..\..\Defines.mak

build: version.res dirs
    $(DELPHI_MSBUILD) oskbulkrenderer.dproj /p:Platform=Win32
    
    $(TDS2DBG) $(WIN32_TARGET_PATH)\oskbulkrenderer.exe
    $(MAKEJCLDBG) $(WIN32_TARGET_PATH)\oskbulkrenderer.map
    $(COPY) $(WIN32_TARGET_PATH)\oskbulkrenderer.exe $(PROGRAM)\support
    if exist $(WIN32_TARGET_PATH)\oskbulkrenderer.dbg $(COPY) $(WIN32_TARGET_PATH)\oskbulkrenderer.dbg $(DEBUGPATH)\support

clean: def-clean
    -rd /s/q Win32

signcode:
    rem Not signing this utility

backup:
    rem No source code backup needed

!include ..\..\Target.mak
