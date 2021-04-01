@echo off
rem
rem This batch script should be called in the teamcity build script to set the current
rem release build version number for Keyman from the teamcity environment
rem

set tmpbuildnumber=%BUILD_NUMBER:.=,%
echo PRODUCTVERSION %tmpbuildnumber% > %KEYMAN_ROOT%\windows\src\version.txt
