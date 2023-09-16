@echo off
echo Compiles the keyboards using the legacy kmcomp.exe
echo to use as baseline comparisons for kmcmplib
rem TODO: we'll need a binary download for kmcomp.exe
rem       We can use the last version available on downloads.keyman.com
rem       see keyboards repo for example of best way to download+extract
for %%d in (*.kmn) do ..\..\..\..\..\bin\kmcomp -no-compiler-version -d %%d