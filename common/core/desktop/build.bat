@echo off

setlocal enabledelayedexpansion

if "%1"=="" goto help
if "%1"=="all" goto all
if "%1"=="x86" goto build
if "%1"=="x64" goto build

echo "Invalid parameter."
goto help

:help
echo Usage: build-core.bat x86^|x64^|all [-c]
goto :eof

:all
setlocal
cd %KEYMAN_ROOT%\common\core\desktop
cmd /c build.bat x86 -i
if errorlevel 1 exit /b !errorlevel!

cd %KEYMAN_ROOT%\common\core\desktop
cmd /c build.bat x64 -i
if errorlevel 1 exit /b !errorlevel!

goto :eof

:build
if "%2"=="-c" goto :setup
set ARCH=%1
echo Building Keyman Core for Windows !ARCH!

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
call !VCVARSALL! !ARCH! 8.1
if errorlevel 1 exit /b !errorlevel!

echo === Calling meson setup ===
cd %KEYMAN_ROOT%\common\core\desktop
meson build-!ARCH! --werror
if errorlevel 1 exit /b !errorlevel!

echo === Building Keyman Core ===
cd build-!ARCH!
if errorlevel 1 exit /b !errorlevel!

ninja
if errorlevel 1 exit /b !errorlevel!

echo === Running tests ===
rem Run test cases immediately as they are quick
meson test --print-errorlogs
if errorlevel 1 exit /b !errorlevel!

cd ..

if "%2"=="-i" goto :eof

:setup

rem Standalone build, so we'll make the environment available to the caller
rem Also setup
endlocal
for /f "usebackq tokens=*" %%i in (`..\..\..\resources\build\vswhere -latest -requires Microsoft.Component.MSBuild -find **\vcvarsall.bat`) do (
  set VCVARSALL="%%i"
)
%VCVARSALL% %1 8.1
goto :eof

rem EOF
