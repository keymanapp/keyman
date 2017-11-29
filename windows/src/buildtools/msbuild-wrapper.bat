@echo off

rem
rem This wrapper is for Delphi msbuild calls only. It sets up the msbuild environment
rem for Delphi, which is not compatible with the VS msbuild environment.
rem
rem Do not use this for VS builds.
rem

call rsvars.bat
msbuild.exe %*
exit /b %errorlevel%
