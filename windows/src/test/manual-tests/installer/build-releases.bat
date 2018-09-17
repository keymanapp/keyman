@echo off
call "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsx86_amd64.bat"

setlocal enableextensions
pushd
set s=%keyman_root%\windows\src
set r=%keyman_root%\windows\release
call :run-build 11 0 1500 0
if errorlevel 1 exit /b %errorlevel%

call :run-build 11 0 1501 0
if errorlevel 1 exit /b %errorlevel%

call :run-build 11 0 1502 0
if errorlevel 1 exit /b %errorlevel%

call :run-build 22 0 900 0
if errorlevel 1 exit /b %errorlevel%

goto end

:run-build
echo ***********************************************************************************************
echo Starting release build of version %1.%2.%3.%4
echo ***********************************************************************************************

echo PRODUCTVERSION %1,%2,%3,%4 > %s%\version.txt
if exist %r%\%1.%2.%3.%4\nul rd /s/q %r%\%1.%2.%3.%4
cd /d %s%
make release
if errorlevel 1 exit /b %errorlevel%

echo ***********************************************************************************************
echo Finishing release build of version %1.%2.%3.%4
echo ***********************************************************************************************

goto :eof

:end
popd
echo done
