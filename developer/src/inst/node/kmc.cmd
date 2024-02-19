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

rem "%*" doesn't work with shift https://stackoverflow.com/a/34005255/1836776
set params=
:build_params
if @%1==@ goto :finished_building_params
set params=%params% %1
shift
goto :build_params
:finished_building_params

"%~dp0\node.js\node.exe" %ESM% "%~dp0\kmc\kmc.mjs" %params%
exit /b %errorlevel%