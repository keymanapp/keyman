@if (@this==@isBatch) @then

@echo off
SETLOCAL ENABLEDELAYEDEXPANSION
setlocal enableextensions

call :scan-files
call :scan-registry

endlocal
exit /b

:scan-files
call :directory_info "%programfiles(x86)%\Keyman"
call :directory_info "%programfiles(x86)%\Common Files\Keyman"
call :directory_info "%programfiles(x86)%\Tavultesoft"
call :directory_info "%programfiles(x86)%\Common Files\Tavultesoft"
rem todo program menu, desktop
goto :eof

:scan-registry
call :registry_info HKCU\Software\Tavultesoft
call :registry_info HKCU\Software\Keyman
call :registry_info HKLM\Software\Wow6432Node\Tavultesoft
call :registry_info HKLM\Software\Wow6432Node\Keyman
rem call :reg "HKCU\Keyboard Layout"
rem call :reg HKCU\Software\Microsoft\CTF
rem call :reg HKLM\Software\Microsoft\CTF
goto :eof

:registry_info

echo ==== Scanning %1 ====

reg query %1 /s 2>nul
goto :eof

:directory_info

echo ==== Scanning %1 ====

dir /s /b %1

echo ==== File Versions in %1 ====

for /r %1 %%f in (*.dll *.exe) do (
  set "file=%%~ff"
  cscript //nologo //e:jscript "%~f0" /file:"!file!"
)
goto :eof

@end

    var file = WScript.Arguments.Named.Item('file').replace(/\\/g,'\\\\');
    var wmi = GetObject('winmgmts:{impersonationLevel=impersonate}!\\\\.\\root\\cimv2')
    var files = new Enumerator(wmi.ExecQuery('Select Name,Version from CIM_datafile where Name = \''+file+'\'')) 

    while (!files.atEnd()){
        WScript.StdOut.WriteLine('"' + files.item().Name + '" ' + files.item().Version);
        files.moveNext();
    };
    WScript.Quit(0)
