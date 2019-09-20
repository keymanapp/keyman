@echo off
setlocal
rem This script is a rough duplicate of /developer/inst/kmlmc.cmd. It is stored here
rem in order to allow TIKE to call out to the compiler while debugging.
if exist "%~dp0..\..\..\..\developer\inst\dist\node.exe" (
  rem If running in windows/src/developer/x/:
  set nodeexe="%~dp0..\..\..\..\developer\inst\dist\node.exe"
  set nodecli="%~dp0..\..\..\..\developer\js\dist\kmlmc.js"
) else if exist "%~dp0..\..\..\developer\inst\dist\node.exe" (
  rem If running in windows/bin/developer/:
  set nodeexe="%~dp0..\..\..\developer\inst\dist\node.exe"
  set nodecli="%~dp0..\..\..\developer\js\dist\kmlmc.js"
) else (
  rem Cannot find node or kmlmc.js relative to execution path
  echo Error: node.exe or kmlmc.js not found.
  exit /b 1
)

%nodeexe% %nodecli% %*
exit /b %errorlevel%
