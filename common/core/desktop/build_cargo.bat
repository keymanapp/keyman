@echo off

setlocal

rem due to some painful cmd-in-bash-in-cmd-in-make-in-cmd interactions
rem causing path confusion for Cargo, it's easier to just call out to a
rem batch file to build our rust targets on Windows

if "%1"=="--debug" (
  set CARGO_TARGET=
  set MESON_TARGET=debug
) else (
  set CARGO_TARGET=--release
  set MESON_TARGET=release
)

call :build_test_rust x86 i686-pc-windows-msvc || exit /b !errorlevel!
call :build_test_rust x64 x86_64-pc-windows-msvc || exit /b !errorlevel!
exit /b 0

:build_test_rust
set TARGETBASE=%1
set TARGET=%2
set TARGET_FLAG=--target=%TARGET%
set TARGET_PATH=%~dp0build

cd "%TARGET_PATH%\..\src\rust" || exit !errorlevel!
cargo build --target-dir="%TARGET_PATH%\rust\%TARGETBASE%" %TARGET_FLAG% %CARGO_TARGET% || exit !errorlevel!

:: On Windows, final output path is ./build/rust/<arch>/<arch_rust>/debug|release/<libraryname>
:: On other platforms, the final file is already in the right place (TARGET=="")

set LIB=rust_mock_processor
set LIBNAME=%LIB%.lib
set BUILT_PATH=%TARGET_PATH%\rust\%TARGETBASE%\%TARGET%\%MESON_TARGET%
set RUST_TARGET_PATH=%TARGET_PATH%\rust\%TARGETBASE%\%MESON_TARGET%

copy "%BUILT_PATH%\%LIBNAME%" "%RUST_TARGET_PATH%\%LIBNAME%" || exit !errorlevel!

goto :eof

