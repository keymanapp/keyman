@echo off

path C:\Windows\Microsoft.NET\Framework\v1.1.4322\
path %path%;C:\Program Files\Borland\BDS\4.0\Bin
path %path%;%SystemRoot%\system32
path %path%;%SystemRoot%
path %path%;%SystemRoot%\System32\Wbem
path %path%;C:\Program Files\Borland\CaliberRM SDK 2005 R2\lib
path %path%;C:\Program Files\Microsoft SQL Server\80\Tools\Binn\
path %path%;C:\Program Files\Microsoft SQL Server\90\Tools\binn\
path %path%;C:\Program Files\Microsoft SQL Server\90\DTS\Binn\
path %path%;C:\Program Files\Microsoft SQL Server\90\Tools\Binn\VSShell\Common7\IDE\
path %path%;C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\PrivateAssemblies\
path %path%;c:\program files\wix
path %path%;c:\bin

@call "c:\program files\microsoft platform sdk\setenv.cmd" > nul

make build signcode
