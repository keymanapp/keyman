@echo off
rem We use test.bat so we can test errorlevels
setlocal
set ESC=

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
  set compiler=..\..\..\..\bin\developer\kmcomp.exe
) else (
  set compiler=%1
)

:test-1
call :should-pass "Valid .kmn, to .js and .kmx, no warnings" test_valid.kmn || goto :eof
call :should-pass ".kps to .kmp, no warnings" test_valid.kps || goto :eof

call :should-fail ".kmn to .kmx, with warning" test_invalid_kmx.kmn || goto :eof
call :should-fail ".kmn to .js, with warning" test_invalid_js.kmn || goto :eof
call :should-fail ".kps to .kmp, with warning" test_invalid.kps || goto :eof

call :should-fail "10.0 .kmn with 14.0-only touch layout codes" test_touchlayout_14_1.kmn || goto :eof
call :should-pass "14.0 .kmn with 14.0 touch layout codes" test_touchlayout_14_2.kmn || goto :eof
call :should-pass "9.0 .kmn with accepted 14.0 touch layout codes" test_touchlayout_14_2.kmn || goto :eof

call :should-fail "#4280: if should be at start of context 1" test_4280_if_start_1.kmn || goto :eof
call :should-fail "#4280: if should be at start of context 2" test_4280_if_start_2.kmn || goto :eof
call :should-fail "#4280: nul should be at start of context 1" test_4280_nul_start_1.kmn || goto :eof
call :should-fail "#4280: nul should be at start of context 2" test_4280_nul_start_2.kmn || goto :eof

call :should-pass "#4423: named code constants, various tests" test_namedcodeconstants.kmn || goto :eof

call :should-pass "#2241: expansions" test_expansion.kmn || goto :eof
call :should-pass "#2241: expansions" test_expansion_1.kmn || goto :eof
:: the two files should be identical.
fc /b test_expansion.kmx test_expansion_1.kmx || goto :eof

call :should-fail "#2241: invalid expansions" test_expansion_invalid.kmn || goto :eof
call :should-fail "#2241: expansion absurdly long" test_expansion_absurd.kmn || goto :eof

call :should-pass "#2241: &CasedKeys" test_casedkeys.kmn || goto :eof
call :should-pass "#2241: &CasedKeys (chars)" test_casedkeys_chars.kmn || goto :eof

call :should-fail "#2241: &CasedKeys (mnemonic 1)" test_casedkeys_mnemonic_1.kmn || goto :eof
call :should-fail "#2241: &CasedKeys (mnemonic 2)" test_casedkeys_mnemonic_2.kmn || goto :eof
call :should-fail "#2241: &CasedKeys (invalid chars 1)" test_casedkeys_invalid_1.kmn || goto :eof
call :should-fail "#2241: &CasedKeys (invalid chars 2)" test_casedkeys_invalid_2.kmn || goto :eof

goto :eof

:should-pass
echo %BLUE%TEST: %1 %WHITE%
"%compiler%" -s -w tests.kpj -t "%2"
if %ERRORLEVEL% EQU 0 (
  echo %GREEN%TEST PASSED%WHITE%
  exit /b 0
)
echo %RED%FAILED: expected %2 to be valid.%WHITE% 1>&2
goto :eof

:should-fail
echo %BLUE%TEST: %1 %WHITE%
"%compiler%" -s -w tests.kpj -t "%2"
if %ERRORLEVEL% GTR 0 (
  echo %GREEN%TEST PASSED%WHITE%
  exit /b 0
)
echo %RED%FAILED: expected %2 to be invalid.%WHITE% 1>&2
exit /b 1

:usage
echo Usage: test.bat [-c] [path-to-kmcomp.exe]
echo -c will add colour via ANSI escapes (don't use in redirected scripts)
echo path-to-kmcomp.exe, if not included will default to ..\..\..\..\bin\developer\kmcomp.exe
