!CMDSWITCHES +S

def-clean:
    $(CLEAN) *.err *.stat *.dproj.local *.Build.CppClean.Log *.suo *.jdbg *.dbg *.dcu *.~* *.dsk *.exe *.rsm *.ncb *.opt *.pch *.plg *.aps *.001 *.sbr *.dep *.drc *.bak *.pdb *.lib *.cod *.ilk *.tds vc80.idb *.map *.bsc version.res manifest.xml manifest.res >nul 2>nul
    $(CLEAN) ExcMagic.Debug *.wixpdb *.identcache *.embed.manifest *.embed.manifest.res *.intermediate.manifest error.log >nul 2>nul
    if exist bin rd /s/q bin
    if exist obj rd /s/q obj

!CMDSWITCHES -S

# This virtual rule forces targets which are folders, e.g. `kmshell` to
# always execute
.virtual:
    rem always execute
