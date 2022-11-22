@echo off
if "%1" == "" goto help

set CONFIG=Release
set BUILD32=0
set BUILD64=0

rem ======================================
rem Startup
rem ======================================

:next_param
if "%1" == "-config" goto set_config
if "%1" == "-32" goto set_32
if "%1" == "-64" goto set_64

rem if "%1" == "-32" set is32=1 && shift
rem if "%1" == "-64" set is64=1 && shift

rem if neither -32 nor -64 specified, then do both
if %BUILD32% == 0 if %BUILD64% == 0 (
  set BUILD32=1
  set BUILD64=1
)

goto %1
goto error

rem ======================================
rem Parse options
rem ======================================

:set_config
shift
set CONFIG=%1
shift
goto next_param

:set_32
set BUILD32=1
shift
goto next_param

:set_64
set BUILD64=1
shift
goto next_param

rem ======================================
rem Iterate through targets
rem ======================================

:next
shift
if "%1" == "" goto :eof
goto %1
goto error


:configure
if %BUILD32% == 1 (
  if exist build_32 (
    rd /s/q build_32
    if errorlevel 1 (
      echo ERROR: Failed to delete build_32 prior to configure
      goto fail
    )
  )
  cmake -B build_32 -DSENTRY_BACKEND=crashpad -A Win32 -DCMAKE_BUILD_TYPE=Release -DSENTRY_BUILD_RUNTIMESTATIC=ON -DCMAKE_SYSTEM_VERSION=6.1
  rem -DCMAKE_SYSTEM_VERSION=6.1 -DCMAKE_VS_WINDOWS_TARGET_PLATFORM_VERSION=8.1
  if errorlevel 1 goto fail
)
if %BUILD64% == 1 (
  if exist build_64 (
    rd /s/q build_64
    if errorlevel 1 (
      echo ERROR: Failed to delete build_64 prior to configure
      goto fail
    )
  )
  cmake -B build_64 -DSENTRY_BACKEND=crashpad -A x64 -DCMAKE_BUILD_TYPE=Release -DSENTRY_BUILD_RUNTIMESTATIC=ON -DCMAKE_SYSTEM_VERSION=6.1
  rem -DCMAKE_SYSTEM_VERSION=6.1 -DCMAKE_VS_WINDOWS_TARGET_PLATFORM_VERSION=8.1
  if errorlevel 1 goto fail
)
goto next


:build
if %BUILD32% == 1 (
  cmake --build build_32 --parallel --config %CONFIG%
  if errorlevel 1 goto fail
)
if %BUILD64% == 1 (
  cmake --build build_64 --parallel --config %CONFIG%
  if errorlevel 1 goto fail
)
goto next


:install
if %BUILD32% == 1 (
  cmake --install build_32 --prefix install_32 --config %CONFIG%
  if errorlevel 1 goto fail
)
if %BUILD64% == 1 (
  cmake --install build_64 --prefix install_64 --config %CONFIG%
  if errorlevel 1 goto fail
)
goto next


:deploy

if "%KEYMAN_ROOT%" == "" (
  echo ERROR: Could not find KEYMAN_ROOT environment variable specifying repository root
  goto fail
)

copy install_32\include\sentry.h %KEYMAN_ROOT%\windows\src\ext\sentry\sentry.h
if errorlevel 1 goto fail

if %BUILD32% == 1 (
  copy install_32\bin\sentry.dll %KEYMAN_ROOT%\windows\src\ext\sentry\sentry.dll
  if errorlevel 1 goto fail
  copy install_32\lib\sentry.lib %KEYMAN_ROOT%\windows\src\ext\sentry\sentry.lib
  if errorlevel 1 goto fail
  copy install_32\bin\crashpad_handler.exe %KEYMAN_ROOT%\windows\src\ext\sentry\crashpad_handler.exe
  if errorlevel 1 goto fail
)

if %BUILD64% == 1 (
  copy install_64\bin\sentry.dll %KEYMAN_ROOT%\windows\src\ext\sentry\sentry.x64.dll
  if errorlevel 1 goto fail
  copy install_64\lib\sentry.lib %KEYMAN_ROOT%\windows\src\ext\sentry\sentry.x64.lib
  if errorlevel 1 goto fail
)

goto next

rem ======================================
rem Help and Errors
rem ======================================

:help
echo Usage: build [-32] [-64] [-config Debug^|Release] [configure] [build] [install] [deploy]
echo If neither -32 nor -64 are specified, do both
echo Default for config is Release
goto :eof

:error
echo Invalid parameters
goto help

:fail
echo FATAL: Build failed.
exit /b 1
