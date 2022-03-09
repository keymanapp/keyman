@echo off

rem Use gosh.bat to launch build.sh
set gosh=%~dp0%..\..\..\resources\gosh\gosh.bat
set buildsh=%~dp0%build.sh
"%gosh%" "%buildsh%" %*

rem If we get this far, then we've failed to launch gosh and/or build.sh
exit /b 2
