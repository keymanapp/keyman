@echo off
::
:: Framework for environment-safe script launches in bash. We should use this instead of a complex git bash command line,
:: for example in our CI. Handles GIT_BASH_FOR_KEYMAN environment variable as well as looking in
:: Program Files / Program Files (x86).
::
:: Usage: gosh.bat script.sh [parameters...]
::

:: Look for a bash launcher
:: first check GIT_BASH_FOR_KEYMAN
if defined GIT_BASH_FOR_KEYMAN set BASHSHELL=%GIT_BASH_FOR_KEYMAN% && goto launch

:: Not found, so check our Program Files
if exist "%ProgramFiles%\git\bin\bash.exe" set BASHSHELL="%ProgramFiles%\git\bin\bash.exe" --init-file "%ProgramFiles%\Git\etc\profile" -l && goto launch
if exist "%ProgramFiles(x86)%\git\bin\bash.exe" set BASHSHELL="%ProgramFiles(x86)%\git\bin\bash.exe" --init-file "%ProgramFiles(x86)%\Git\etc\profile" -l && goto launch

:: No luck. We don't to use any old bash on the path because it could be WSL which will break in other ways at this point
echo Could not find bash. Ensure Git for Windows is installed and optionally set GIT_BASH_FOR_KEYMAN variable:
echo SET GIT_BASH_FOR_KEYMAN="%ProgramFiles%\git\bin\bash.exe" --init-file "%ProgramFiles%\Git\etc\profile" -l

exit /b 1

:launch
:: launch the child script
%BASHSHELL% %*
exit /b %errorlevel%
