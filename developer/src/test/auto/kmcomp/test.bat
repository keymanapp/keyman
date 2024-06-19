@echo off
rem We use test.bat so we can test errorlevels
setlocal
setlocal enabledelayedexpansion
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

if "%1"=="-t" (
  set test=%2
  shift
  shift
)

if "%1"=="" (
  set compiler=..\..\..\..\bin\kmc.cmd
) else (
  set compiler=%1
  shift
)

if not "!test!" == "" goto :test-!test!

:test-1
call :should-pass "Valid .kmn, to .js and .kmx, no warnings" test_valid.kmn || exit /b 1
call :should-pass ".kps to .kmp, no warnings" test_valid.kps || exit /b 2

call :should-fail ".kmn to .kmx, with warning" test_invalid_kmx.kmn || exit /b 3
call :should-fail ".kmn to .js, with warning" test_invalid_js.kmn || exit /b 4
call :should-fail ".kps to .kmp, with warning" test_invalid.kps || exit /b 5

call :should-fail "10.0 .kmn with 14.0-only touch layout codes" test_touchlayout_14_1.kmn || exit /b 6
call :should-pass "14.0 .kmn with 14.0 touch layout codes" test_touchlayout_14_2.kmn || exit /b 7
call :should-pass "9.0 .kmn with accepted 14.0 touch layout codes" test_touchlayout_14_2.kmn || exit /b 8

:test-4280
call :should-fail "#4280: if should be at start of context 1" test_4280_if_start_1.kmn || exit /b 9
call :should-fail "#4280: if should be at start of context 2" test_4280_if_start_2.kmn || exit /b 10
call :should-fail "#4280: nul should be at start of context 1" test_4280_nul_start_1.kmn || exit /b 11
call :should-fail "#4280: nul should be at start of context 2" test_4280_nul_start_2.kmn || exit /b 12
if not "!test!" == "" goto :eof

:test-4423
call :should-pass "#4423: named code constants, various tests" test_namedcodeconstants.kmn || exit /b 13
if not "!test!" == "" goto :eof

:test-2241
call :should-pass "#2241: expansions" test_expansion.kmn || exit /b 14
call :should-pass "#2241: expansions" test_expansion_1.kmn || exit /b 15
:: the two files should be identical.
fc /b test_expansion.kmx test_expansion_1.kmx || exit /b 16

call :should-fail "#2241: invalid expansions" test_expansion_invalid.kmn || exit /b 17
call :should-fail "#2241: expansion absurdly long" test_expansion_absurd.kmn || exit /b 18

call :should-pass "#2241: &CasedKeys" test_casedkeys.kmn || exit /b 19
call :should-pass "#2241: &CasedKeys (chars)" test_casedkeys_chars.kmn || exit /b 20

call :should-fail "#2241: &CasedKeys (mnemonic 1)" test_casedkeys_mnemonic_1.kmn || exit /b 21
call :should-fail "#2241: &CasedKeys (mnemonic 2)" test_casedkeys_mnemonic_2.kmn || exit /b 22
call :should-fail "#2241: &CasedKeys (mnemonic 3)" test_casedkeys_mnemonic_3.kmn || exit /b 23
call :should-fail "#2241: &CasedKeys (invalid chars 1)" test_casedkeys_invalid_1.kmn || exit /b 24
call :should-fail "#2241: &CasedKeys (invalid chars 2)" test_casedkeys_invalid_2.kmn || exit /b 25
if not "!test!" == "" goto :eof

:test-5963
call :should-pass "#5963 start-of-sentence" test_5963_start_of_sentence.kmn || exit /b 26
call :should-fail "#5963 begin newcontext (missing group)" test_5963_newcontext_1.kmn || exit /b 27
call :should-fail "#5963 begin newcontext (not readonly)" test_5963_newcontext_2.kmn || exit /b 28
call :should-fail "#5963 begin postkeystroke (missing group)" test_5963_postkeystroke_1.kmn || exit /b 29
call :should-fail "#5963 begin postkeystroke (not readonly) " test_5963_postkeystroke_2.kmn || exit /b 30
call :should-fail "#5963 context not first token in readonly group output" test_5963_readonlygroup_misplacedcontext.kmn || exit /b 31
call :should-fail "#5963 emitting chars in readonly group" test_5963_readonlygroup_output.kmn || exit /b 32
call :should-fail "#5963 using non-readonly group in readonly group" test_5963_readonlygroup_usenonreadonly.kmn || exit /b 33
if not "!test!" == "" goto :eof

:test-6440
:: CHINT_UnreachableRule from compiler.rc
set CHINT_UnreachableRule="This rule will never be matched as another rule takes precedence"
call :should-have-message "#6440 hint on unreachable code #1" test_6440_unreachable_code_1.kmn %CHINT_UnreachableRule% || exit /b 34
call :should-have-message "#6440 hint on unreachable code #2" test_6440_unreachable_code_2.kmn %CHINT_UnreachableRule% || exit /b 35
call :should-have-message "#6440 hint on unreachable code #3" test_6440_unreachable_code_3.kmn %CHINT_UnreachableRule% || exit /b 36
call :should-have-message "#6440 hint on unreachable code #4" test_6440_unreachable_code_4.kmn %CHINT_UnreachableRule% || exit /b 37
call :should-pass "#6440 hint on unreachable code #5" test_6440_unreachable_code_5.kmn || exit /b 38
if not "!test!" == "" goto :eof

:test-194
::
call :should-have-message -f test_194_filename_case.out.txt "#194 filename case" test_194_filename_case.kmn || exit /b 34
if not "!test!" == "" goto :eof

call :should-fail "#6462 duplicate group #1" test_6462_duplicate_group_1.kmn || exit /b 39
call :should-fail "#6462 duplicate group #2" test_6462_duplicate_group_2.kmn || exit /b 40
call :should-fail "#6462 duplicate store #1" test_6462_duplicate_store_1.kmn || exit /b 41
call :should-fail "#6462 duplicate store #2" test_6462_duplicate_store_2.kmn || exit /b 42
call :should-fail "#6462 duplicate store #3" test_6462_duplicate_store_3.kmn || exit /b 42

goto :eof
:: --------------------------------------------------------------

:should-pass
echo %BLUE%TEST: %1 %WHITE%
call "%compiler%" build --no-color --log-level hint --compiler-warnings-as-errors "%2"
if !ERRORLEVEL! EQU 0 (
  echo %GREEN%TEST PASSED%WHITE%
  exit /b 0
)
echo %RED%FAILED: expected %2 to be valid.%WHITE% 1>&2
exit /b 1

:should-fail
echo %BLUE%TEST: %1 %WHITE%
call "%compiler%" build --no-color --log-level hint --compiler-warnings-as-errors  "%2"
if !ERRORLEVEL! GTR 0 (
  echo %GREEN%TEST PASSED%WHITE%
  exit /b 0
)
echo %RED%FAILED: expected %2 to be invalid.%WHITE% 1>&2
exit /b 1

:should-have-message

if %1 == -f (
  set outfile=%2
  shift
  shift
) else (
  set outfile=
)

echo %BLUE%TEST: %1 %WHITE%

if exist error.log del error.log
call "%compiler%" build --no-color --log-level hint --compiler-warnings-as-errors "%2" > error.log
if !ERRORLEVEL! GTR 0 (
  echo %RED%FAILED: expected %2 to be a valid keyboard.%WHITE% 1>&2
  echo ---------------------------------------------------------------------------
  echo %BLUE%## error.log%WHITE%
  type error.log
  echo ---------------------------------------------------------------------------
  del error.log
  exit /b 1
)

if "!outfile!" == "" (
  findstr /L /C:%3 error.log > nul
  if !ERRORLEVEL! GTR 0 (
    echo %RED%FAILED: expected compile results to contain message %3.%WHITE% 1>&2
    echo ---------------------------------------------------------------------------
    echo %BLUE%## error.log%WHITE%
    type error.log
    echo ---------------------------------------------------------------------------
    del error.log
    exit /b 1
  )
) else (
  fc error.log !outfile! > nul
  if !ERRORLEVEL! GTR 0 (
    echo %RED%FAILED: expected output in error.log did not match !outfile!.%WHITE% 1>&2
    echo ---------------------------------------------------------------------------
    echo %BLUE%## error.log%WHITE%
    type error.log
    echo ---------------------------------------------------------------------------
    echo %BLUE%## !outfile!%WHITE%
    type !outfile!
    echo ---------------------------------------------------------------------------
    del error.log
    exit /b 1
  )
)

del error.log
echo %GREEN%TEST PASSED%WHITE%
exit /b 0

:usage
echo Usage: test.bat [-c] [path-to-kmc.cmd]
echo -c will add colour via ANSI escapes (don't use in redirected scripts)
echo path-to-kmc.cmd, if not included will default to ..\..\..\..\bin\kmc.cmd
