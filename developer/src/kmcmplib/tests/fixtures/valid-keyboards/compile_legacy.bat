@echo off
echo Compiles the keyboards using the legacy kmcomp.exe
echo to use as baseline comparisons for kmcmplib
for %%d in (*.kmn) do ..\..\..\..\..\bin\kmcomp -no-compiler-version -d %%d