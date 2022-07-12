@echo off
setlocal
rem This script is a rough duplicate of /developer/src/node/inst/kmlmp.cmd. It is stored here
rem in order to allow TIKE to call out to the compiler while debugging.
if exist "%~dp0..\..\..\..\developer\src\inst\node\dist\node.exe" (
  rem If running in windows/src/developer/x/:
  set nodeexe="%~dp0..\..\..\..\developer\src\inst\node\dist\node.exe"
  set nodecli="%~dp0..\..\..\..\developer\src\kmlmc\dist\kmlmp.js"
) else if exist "%~dp0..\..\..\developer\src\inst\node\dist\node.exe" (
  rem If running in windows/bin/developer/:
  set nodeexe="%~dp0..\..\..\developer\src\inst\node\dist\node.exe"
  set nodecli="%~dp0..\..\..\developer\src\kmlmc\dist\kmlmp.js"
) else (
  rem Cannot find node or kmlmp.js relative to execution path
  echo Error: node.exe or kmlmp.js not found.
  exit /b 1
)

%nodeexe% %nodecli% %*
exit /b %errorlevel%
