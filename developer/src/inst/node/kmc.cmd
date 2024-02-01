@echo off
rem This script avoids path dependencies for node for distribution
rem with Keyman Developer. When used on platforms other than Windows,
rem node can be used directly with the compiler (`npm link` will setup).
if "%1" == "--enable-source-maps" (
  set ESM=--enable-source-maps
  shift
) else (
  set ESM=
)
"%~dp0\node.js\node.exe" %ESM% "%~dp0\kmc\kmc.mjs" %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %errorlevel%