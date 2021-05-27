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
echo Usage: %0 x86^|x64^|all debug^|release [build] [tests]
echo   or
echo Usage: %0 x86^|x64^|all -c
echo -c will leave your environment configured for Visual Studio for selected platform.
echo -c can be used only with x86 and x64 options
echo.
echo Otherwise, %0 is intended to be used by build.sh, not directly.
echo At least one of 'build' or 'tests' is required.
goto :eof

rem ----------------------------------

:all

setlocal
cd %KEYMAN_ROOT%\common\core\desktop
cmd /c build.bat x86 %2 %3 %4 %5 || exit !errorlevel!

cd %KEYMAN_ROOT%\common\core\desktop
cmd /c build.bat x64 %2 %3 %4 %5 || exit !errorlevel!

goto :eof

rem ----------------------------------

:build

if "%2"=="-c" goto :setup

set ARCH=%1

echo === Locating Visual Studio ===

rem From https://github.com/microsoft/vswhere
for /f "usebackq tokens=*" %%i in (`..\..\..\resources\build\vswhere -latest -requires Microsoft.Component.MSBuild -find **\vcvarsall.bat`) do (
  set VCVARSALL="%%i"
)

if errorlevel 1 (
  echo vswhere failed [!errorlevel!]
  exit /b !errorlevel!
)

if not exist "!VCVARSALL!" (
  echo Could not find vcvarsall.bat [!VCVARSALL!]
  exit /b 1
)

echo === Configuring VC++ ===
call !VCVARSALL! !ARCH! || exit !errorlevel!

cd %KEYMAN_ROOT%\common\core\desktop

set BUILDTYPE=%2

if "%3" == "build" (
  echo === Calling meson build for Windows !ARCH! !BUILDTYPE! ===
  if exist build\!ARCH!\!BUILDTYPE! rd /s/q build\!ARCH!\!BUILDTYPE!
  meson build\!ARCH!\!BUILDTYPE! --buildtype !BUILDTYPE! --werror || exit !errorlevel!

  echo === Building Keyman Core for Windows !ARCH! !BUILDTYPE! ===
  cd build\!ARCH!\!BUILDTYPE! || exit !errorlevel!

  ninja || exit !errorlevel!
  cd ..\..\..

  shift
)

if "%3" == "tests" (
  cd build\!ARCH!/!BUILDTYPE! || exit !errorlevel!

  echo === Running tests for Windows !ARCH! !BUILDTYPE! ===
  meson test --print-errorlogs || exit !errorlevel!

  cd ..\..\..
)

goto :eof

rem ----------------------------------

:setup

rem Standalone build, so we'll make the environment available to the caller
rem Also setup
endlocal
for /f "usebackq tokens=*" %%i in (`..\..\..\resources\build\vswhere -latest -requires Microsoft.Component.MSBuild -find **\vcvarsall.bat`) do (
  set VCVARSALL="%%i"
)
%VCVARSALL% %1
goto :eof

rem ----------------------------------
