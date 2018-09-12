rem make kmcomp and copy to keyboards repo
rem DO NOT COMMMIT
make kmcomp

if %ERRORLEVEL% EQU 0 copy /Y c:\src\keymanapp\windows\bin\developer\kmcomp.exe c:\src\keyboards\tools\
