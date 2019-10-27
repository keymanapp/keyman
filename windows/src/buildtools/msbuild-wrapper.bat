@echo off

rem
rem This wrapper is for Delphi msbuild calls only. It sets up the msbuild environment
rem for Delphi, which is not compatible with the VS msbuild environment.
rem
rem Do not use this for VS builds.
rem

if not exist "%DCC32PATH%\rsvars.bat" echo Batch file %DCC32PATH%\rsvars.bat does not exist && exit /b 1

call "%DCC32PATH%\rsvars.bat"
msbuild.exe %*
exit /b %errorlevel%
