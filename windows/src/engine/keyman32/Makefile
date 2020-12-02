#
# Keyman32 Makefile
#

!include ..\..\Defines.mak

build: version.res dirs
    $(MSBUILD) keyman32.vcxproj $(MSBUILD_BUILD)
    $(COPY) keyman32.dll $(PROGRAM)\engine
    $(COPY) keyman32.pdb $(DEBUGPATH)\engine
    $(COPY) keyman32.lib $(OUTLIB)

clean: def-clean
    $(MSBUILD) $(MSBUILD_CLEAN) keyman32.vcxproj
    -del x86\release\buildlog.htm
    -rmdir x86\release
    -del x86\debug\buildlog.htm
    -rmdir x86\debug
    -rmdir x86

signcode:
    $(SIGNCODE) /d "Keyman Engine" $(PROGRAM)\engine\keyman32.dll

wrap-symbols:
    $(SYMSTORE) $(PROGRAM)\engine\keyman32.dll /t keyman-engine-windows
    $(SYMSTORE) $(DEBUGPATH)\engine\keyman32.pdb /t keyman-engine-windows

backup:
    $(WZZIP) $(BUILD)\engine\keyman32.zip  $(BACKUPDEFAULTS) keyman32.dll

install:
    $(COPY) $(PROGRAM)\engine\keyman32.dll "$(INSTALLPATH_KEYMANENGINE)\keyman32.dll"
    echo You may want to manually tweak keyman-debug-etw.man and fill in $(INSTALLPATH_KEYMANENGINE)\keyman32.dll
    echo and then run wevtutil im keyman-debug-etw.man to get the latest event tracing

!include ..\..\Target.mak
