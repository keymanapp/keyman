CLEAN=@-del /S /Q

def-clean:
    $(CLEAN) *.err *.stat *.dproj.local *.Build.CppClean.Log *.suo *.jdbg *.dbg *.dcu *.~* *.dsk *.exe *.rsm *.ncb *.opt *.pch *.plg *.aps *.001 *.sbr *.dep *.drc *.bak *.pdb *.lib *.cod *.ilk *.tds vc80.idb *.map *.bsc version.rc version.res manifest.xml manifest.res >nul 2>nul
    $(CLEAN) ExcMagic.Debug *.wixpdb *.identcache *.embed.manifest *.embed.manifest.res *.intermediate.manifest error.log >nul 2>nul

versionhistory-app: dirs
    $(MAKE) versionhistory-app2

versionhistory-app2:
    cd $(ROOT)\src\buildtools
    $(MAKE) versionhistory

mkver-app: dirs
    cd $(ROOT)\src\buildtools\mkver
    $(MAKE)
    cd $(ROOT)\src

devtools-app: dirs
    cd $(ROOT)\src\buildtools
    $(MAKE) devtools
    cd $(ROOT)\src

version.res: version.in
    $(MKVER_V)
    rc version.rc

manifest.res: version.res
    $(MKVER_M)
    rc manifest.rc

dirs:
    @-mkdir $(ROOT)\build\desktop 2>nul
    @-mkdir $(ROOT)\build\engine 2>nul
    @-mkdir $(ROOT)\build\developer 2>nul
    @-mkdir $(ROOT)\build\buildtools 2>nul
    @-mkdir $(ROOT)\build\online 2>nul
    @-mkdir $(ROOT)\build\support 2>nul
    @-mkdir $(ROOT)\build\inst 2>nul

    @-mkdir $(ROOT)\bin\developer\samples 2>nul
    @-mkdir $(ROOT)\bin\developer\xml 2>nul
    @-mkdir $(ROOT)\bin\buildtools 2>nul
    @-mkdir $(ROOT)\bin\engine 2>nul
    @-mkdir $(ROOT)\bin\desktop 2>nul
    @-mkdir $(ROOT)\bin\desktop\xml 2>nul
    @-mkdir $(ROOT)\bin\support 2>nul
    @-mkdir $(ROOT)\bin\online 2>nul
    @-mkdir $(ROOT)\bin\inst 2>nul
    @-mkdir $(ROOT)\bin\help 2>nul
    @-mkdir $(ROOT)\bin\help\desktop 2>nul
    @-mkdir $(ROOT)\bin\help\developer 2>nul
    @-mkdir $(ROOT)\bin\help\php 2>nul
    @-mkdir $(ROOT)\bin\help\php\desktop 2>nul
    @-mkdir $(ROOT)\bin\help\php\developer 2>nul
    @-mkdir $(ROOT)\lib 2>nul
    @-mkdir $(ROOT)\release 2>nul

    @-mkdir $(ROOT)\debug\developer 2>nul
    @-mkdir $(ROOT)\debug\buildtools 2>nul
    @-mkdir $(ROOT)\debug\engine 2>nul
    @-mkdir $(ROOT)\debug\desktop 2>nul
    @-mkdir $(ROOT)\debug\support 2>nul
    @-mkdir $(ROOT)\debug\online 2>nul
    @-mkdir $(ROOT)\debug\inst 2>nul

vcvars32:
    vcvars32.bat
