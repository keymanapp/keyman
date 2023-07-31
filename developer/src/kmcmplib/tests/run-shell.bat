@echo off
if defined GIT_BASH_FOR_KEYMAN (
  set GIT_BASH=%GIT_BASH_FOR_KEYMAN%
) else (
  set GIT_BASH="%ProgramFiles%\git\bin\bash.exe" --init-file "%ProgramFiles%\Git\etc\profile" -l
)
%GIT_BASH% %1 %2 %3 %4 %5 %6 %7 %8 %9