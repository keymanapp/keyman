!CMDSWITCHES +S

def-clean:
    $(CLEAN) *.err *.stat *.dproj.local *.Build.CppClean.Log *.suo *.jdbg *.dbg *.dcu *.~* *.dsk *.exe *.rsm *.ncb *.opt *.pch *.plg *.aps *.001 *.sbr *.dep *.drc *.bak *.pdb *.lib *.cod *.ilk *.tds vc80.idb *.map *.bsc version.res manifest.xml manifest.res >nul 2>nul
    $(CLEAN) ExcMagic.Debug *.wixpdb *.identcache *.embed.manifest *.embed.manifest.res *.intermediate.manifest error.log >nul 2>nul
    if exist bin rd /s/q bin
    if exist obj rd /s/q obj

!CMDSWITCHES -S

versionhistory-app: dirs
    $(MAKE) versionhistory-app2

versionhistory-app2:
    cd $(ROOT)\src\buildtools
    $(MAKE) versionhistory

devtools-app: dirs
    cd $(COMMON_ROOT)
    @if not exist $(KEYMAN_ROOT)\include\keymanversion_build.h $(MAKE) global-versions
    cd $(COMMON_ROOT)\tools\devtools
    $(MAKE)
    cd $(ROOT)\src

version.res: version.rc
    rc $?

manifest.res: manifest.in version.res
    $(MKVER_M)
    rc manifest.rc

!CMDSWITCHES +S

dirs:
    -mkdir $(ROOT)\build\desktop 2>nul
    -mkdir $(ROOT)\build\engine 2>nul
    -mkdir $(KEYMAN_ROOT)\developer\build 2>nul
    -mkdir $(ROOT)\build\buildtools 2>nul
    -mkdir $(ROOT)\build\online 2>nul
    -mkdir $(ROOT)\build\support 2>nul
    -mkdir $(ROOT)\build\inst 2>nul

    -mkdir $(KEYMAN_ROOT)\developer\bin 2>nul
    -mkdir $(KEYMAN_ROOT)\developer\bin\samples 2>nul
    -mkdir $(KEYMAN_ROOT)\developer\bin\xml 2>nul
    -mkdir $(KEYMAN_ROOT)\developer\bin\projects 2>nul
    -mkdir $(KEYMAN_ROOT)\developer\bin\projects\templates 2>nul
    -mkdir $(ROOT)\bin\buildtools 2>nul
    -mkdir $(ROOT)\bin\engine 2>nul
    -mkdir $(ROOT)\bin\desktop 2>nul
    -mkdir $(ROOT)\bin\desktop\xml 2>nul
    -mkdir $(ROOT)\bin\support 2>nul
    -mkdir $(ROOT)\bin\online 2>nul
    -mkdir $(ROOT)\bin\inst 2>nul
    -mkdir $(ROOT)\bin\help 2>nul
    -mkdir $(ROOT)\bin\help\desktop 2>nul
    -mkdir $(ROOT)\bin\help\developer 2>nul
    -mkdir $(ROOT)\bin\help\md 2>nul
    -mkdir $(ROOT)\bin\help\md\desktop 2>nul
    -mkdir $(ROOT)\lib 2>nul
    -mkdir $(KEYMAN_ROOT)\developer\lib 2>nul
    -mkdir $(ROOT)\release 2>nul
    -mkdir $(KEYMAN_ROOT)\developer\release 2>nul

    -mkdir $(KEYMAN_ROOT)\developer\debug 2>nul
    -mkdir $(ROOT)\debug\buildtools 2>nul
    -mkdir $(ROOT)\debug\engine 2>nul
    -mkdir $(ROOT)\debug\desktop 2>nul
    -mkdir $(ROOT)\debug\support 2>nul
    -mkdir $(ROOT)\debug\online 2>nul
    -mkdir $(ROOT)\debug\inst 2>nul

!CMDSWITCHES -S

vcvars32:
    vcvars32.bat

# This command injects the current version and tier into the VERSION_WIN and VERSION_TIER defines and then
# calls "make wrap-symbols", but it only runs if the symbols folder exists

!IFNDEF HEADER_MAK
# we don't want this target for Makefiles with Header.mak, as they just call into sub-projects
symbols:
    if exist $(KEYMAN_SYMSTOREPATH) \
      cmd /C \
        for /f %v in ($(KEYMAN_ROOT)\VERSION.md) do \
          for /f %t in ($(KEYMAN_ROOT)\TIER.md) do \
            $(MAKE) "VERSION_TIER=%t" "VERSION_WIN=%v" wrap-symbols
!ENDIF

# This virtual rule forces targets which are folders, e.g. `kmshell` to
# always execute
.virtual:
    rem always execute
