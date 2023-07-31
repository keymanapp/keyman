@echo off
rem ****************************************************************
rem Run build.sh, not this file directly, to build on Windows.
rem ****************************************************************
setlocal enabledelayedexpansion

if "%1"=="" goto help
if "%1"=="all" goto all
if "%1"=="x86" goto build
if "%1"=="x64" goto build

echo "Invalid parameter."
goto help

rem ----------------------------------

:help
echo Usage: %0 x86^|x64^|all debug^|release [configure] [build] [test] [additional params for meson/ninja]
echo   or
echo Usage: %0 x86^|x64 -c
echo -c will leave your environment configured for Visual Studio for selected platform.
echo.
echo Otherwise, %0 is intended to be used by build.sh, not directly.
echo At least one of 'configure', 'build', or 'test' is required.
goto :eof

rem ----------------------------------

:all

setlocal
cd %KEYMAN_ROOT%\developer\src\kmcmplib
cmd /c build.bat x86 %2 %3 %4 %5 %6 %7 %8 %9 || exit !errorlevel!

cd %KEYMAN_ROOT%\developer\src\kmcmplib
cmd /c build.bat x64 %2 %3 %4 %5 %6 %7 %8 %9 || exit !errorlevel!

goto :eof

rem ----------------------------------

:build

set ARCH=%1
shift

if "%1"=="-c" goto :setup

echo === Locating Visual Studio ===

rem From https://github.com/microsoft/vswhere
for /f "usebackq delims=#" %%a in (`"%programfiles(x86)%\Microsoft Visual Studio\Installer\vswhere" -latest -property installationPath`) do (
  set VsDevCmd_Path=%%a\Common7\Tools\VsDevCmd.bat
)

if errorlevel 1 (
  echo vswhere failed [!errorlevel!]
  exit /b !errorlevel!
)

if not exist "!VsDevCmd_Path!" (
  echo Could not find vsdevcmd.bat [!VsDevCmd_Path!]
  exit /b 1
)

echo === Configuring VC++ ===
set VSCMD_SKIP_SENDTELEMETRY=1
call "!VsDevCmd_Path!" -arch=!ARCH! -no_logo -startdir=none || exit !errorlevel!

cd %KEYMAN_ROOT%\developer\src\kmcmplib

set BUILDTYPE=%1
shift

set STATIC_LIBRARY=--default-library both

set COMMAND=%1
shift

if "!COMMAND!" == "configure" (
  echo === Configuring Keyman KMX Compiler for Windows !ARCH! !BUILDTYPE! ===
  if exist build\!ARCH!\!BUILDTYPE! rd /s/q build\!ARCH!\!BUILDTYPE!
  meson setup build\!ARCH!\!BUILDTYPE! !STATIC_LIBRARY! --buildtype !BUILDTYPE! --werror %1 %2 %3 %4 %5 %6 %7 %8 %9 || exit !errorlevel!
  shift
)

if "!COMMAND!" == "build" (
  echo === Building Keyman KMX Compiler for Windows !ARCH! !BUILDTYPE! ===
  cd build\!ARCH!\!BUILDTYPE! || exit !errorlevel!
  ninja %1 %2 %3 %4 %5 %6 %7 %8 %9 || exit !errorlevel!
  cd ..\..\..
  shift
)

if "!COMMAND!" == "test" (
  echo === Testing Keyman KMX Compiler for Windows !ARCH! !BUILDTYPE! ===
  cd build\!ARCH!\!BUILDTYPE! || exit !errorlevel!
  meson test --print-errorlogs %1 %2 %3 %4 %5 %6 %7 %8 %9 || exit !errorlevel!
  cd ..\..\..
  shift
)

goto :eof

rem ----------------------------------

:setup

rem Standalone build, so we'll make the environment available to the caller
rem Also setup
rem Note: Visual Studio 2022 doesn't provide vcvarsall.bat, so we'll have to find a different solution
endlocal
for /f "usebackq delims=#" %%a in (`"%programfiles(x86)%\Microsoft Visual Studio\Installer\vswhere" -latest -property installationPath`) do (
  set VsDevCmd_Path=%%a\Common7\Tools\VsDevCmd.bat
)

set VSCMD_SKIP_SENDTELEMETRY=1
"!VsDevCmd_Path!" -arch=!ARCH! -no_logo -startdir=none
goto :eof

rem ----------------------------------
