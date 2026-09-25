@echo off
rem Assuming debug build, running from same folder

echo ---- ARM64 ----
if %PROCESSOR_ARCHITECTURE% == ARM64 ( .\bin\ARM64\Debug\wow64kbd.exe ) else ( echo Skipping because arch=%PROCESSOR_ARCHITECTURE% )
echo ---- ARM64EC ----
if %PROCESSOR_ARCHITECTURE% == ARM64 ( .\bin\ARM64EC\Debug\wow64kbd.exe ) else ( echo Skipping because arch=%PROCESSOR_ARCHITECTURE% )
echo ---- x64 ----
.\bin\x64\Debug\wow64kbd.exe
echo ---- x86 (Win32) ----
.\bin\Win32\Debug\wow64kbd.exe
