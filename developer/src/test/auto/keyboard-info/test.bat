@echo off
rem We use test.bat so we can test errorlevels
setlocal
set ESC=

rem This is temporary until source keyboard_info files go away
copy "%KEYMAN_ROOT%\common\schemas\keyboard_info\keyboard_info.schema.json" "%KEYMAN_ROOT%\common\schemas\keyboard_info\keyboard_info.source.json"

if "%1"=="-h" goto usage
if "%1"=="--help" goto usage
if "%1"=="-?" goto usage

if "%1"=="-c" (
  set RED=%ESC%[1;31m
  set GREEN=%ESC%[1;32m
  set WHITE=%ESC%[0;37m
  set BLUE=%ESC%[1;36m
  shift
)
if "%1"=="" (
  set compiler=..\..\..\..\bin\kmcomp.exe
) else (
  set compiler=%1
)

:test-1
call :should-pass "Valid keyboard info file" test-valid.keyboard_info || exit /b 1

call :should-fail "Keyboard info file with invalid BCP-47 code" test-invalid-language-code.keyboard_info || exit /b 1

:: in 13.0, this test was a should-fail
:: in 14.0, we made non-canonical bcp 47 codes a hint instead of an error
:: see #4689.
call :should-pass "Keyboard info file with non-canonical BCP-47 code" test-non-canonical-language-code.keyboard_info || exit /b 1

goto :eof

:should-pass
echo %BLUE%TEST: %1 %WHITE%
"%compiler%" -vs -schema-path "%KEYMAN_ROOT%\common\schemas\keyboard_info" "%2"
if %ERRORLEVEL% EQU 0 (
  echo %GREEN%TEST PASSED%WHITE%
  exit /b 0
)
echo %RED%FAILED: expected %2 to be valid.%WHITE% 1>&2
exit /b 1

:should-fail
echo %BLUE%TEST: %1 %WHITE%
"%compiler%" -s -vs -schema-path "%KEYMAN_ROOT%\common\schemas\keyboard_info" "%2"
if %ERRORLEVEL% GTR 0 (
  echo %GREEN%TEST PASSED%WHITE%
  exit /b 0
)
echo %RED%FAILED: expected %2 to be invalid.%WHITE% 1>&2
exit /b 1

:usage
echo Usage: test.bat [-c] [path-to-kmcomp.exe]
echo -c will add colour via ANSI escapes (don't use in redirected scripts)
echo path-to-kmcomp.exe, if not included will default to ..\..\..\..\bin\kmcomp.exe
