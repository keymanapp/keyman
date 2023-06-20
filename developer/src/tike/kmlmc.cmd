@echo off
setlocal
rem This script is based on /developer/src/node/inst/kmlmc.cmd. It is stored here
rem in order to allow TIKE to call out to the compiler while debugging.
if exist "%~dp0..\inst\node\dist\node.exe" (
  rem If running in developer/src/tike/:
  set nodeexe="%~dp0..\inst\node\dist\node.exe"
  set nodecli="%~dp0..\kmc\build\src\kmlmc.js"
) else if exist "%~dp0..\src\inst\node\dist\node.exe" (
  rem If running in developer/bin/:
  set nodeexe="%~dp0..\src\inst\node\dist\node.exe"
  set nodecli="%~dp0..\src\kmc\build\src\kmlmc.js"
) else (
  rem Cannot find node or kmlmc.js relative to execution path
  echo Error: node.exe or kmlmc.js not found.
  exit /b 1
)

%nodeexe% --enable-source-maps %nodecli% %*
exit /b %errorlevel%
