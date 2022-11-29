@echo off
dir  /s/b ..\..\..\..\keyboards\release\*.kmn | more | findstr /v /i "\ahom_star.kmn" | findstr /v /i  "\batak.kmn"  
pause