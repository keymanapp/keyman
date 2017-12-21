@echo off
rem 
rem Compile keymanweb and copy compiled javascript and resources to output/embedded folder
rem
rem Note: any changes to this script should be replicated in build.sh
rem

echo Node.js + dependencies check
call npm install

if %errorlevel% neq 0 exit %errorlevel%

rem Definition of global compile constants
set WEB_OUTPUT="..\output"
set EMBED_OUTPUT="..\embedded"
set INTERMEDIATE="..\build"
set SOURCE="."
set NODE_SOURCE="source"

rem Get build version -- if not building in TeamCity, then always use 300

if "%BUILD_COUNTER%"=="" (
  set BUILD=300
) else (
  set BUILD=%BUILD_COUNTER%
)

if "%CLOSURECOMPILERPATH%"=="" set CLOSURECOMPILERPATH=..\node_modules\google-closure-compiler
if "%JAVA%"=="" set JAVA=java

set minifier=%CLOSURECOMPILERPATH%\compiler.jar
set minifier_warnings=--jscomp_error=* --jscomp_off=lintChecks --jscomp_off=unusedLocalVariables
set minifycmd="%JAVA%" -jar "%minifier%" %minifier_warnings%

if not exist %minifier% (
  echo File %minifier% does not exist: have you set the environment variable CLOSURECOMPILERPATH?
  exit /B 1
)

setlocal EnableDelayedExpansion
(set \n=^
%=Do not remove this line=%
)

set REL_TSC_PATH=..\node_modules\.bin
set ABS_TSC_PATH=

rem // Obtain absolute path to local Typescript installation.
pushd %REL_TSC_PATH%
set ABS_TSC_PATH=%CD%
popd

set PATH=%ABS_TSC_PATH%;%PATH%
set NODE_PATH=%ABS_TSC_PATH%

set compiler=npm run tsc --
set compilecmd=%compiler%

goto main

:minify
  set basefile=%1
  set basefile=%basefile:"=%
  set "mapfile=%basefile%.map"

  set outPath=%2
  set outPath=%outPath:"=%

  if [%4]==[] (
    set defines=
  ) else if NOT [%4]==[""] (
    set "defines=--define %4"
  ) else (
    set defines=
  )

  if [%5]==[] (
    set wrapper="%%output%%"
  ) else if not [%5]==[""] (
    set wrapper=%5
  ) else (
    set wrapper="%%output%%"
  )

  set intermed=%INTERMEDIATE:"=%

  set wrapper=%wrapper:"=%
  set wrapper="%wrapper%//# sourceMappingURL=%mapfile%"

  rem The --source_map_input path argument's formatting requires double-quotes.
  %minifycmd% %defines% --source_map_input "%intermed%\%basefile%|%intermed%\%mapfile%" ^
      --create_source_map %outPath%\%mapfile% --js %intermed%\%basefile% --compilation_level %3 ^
      --js_output_file %outPath%\%basefile% --warning_level VERBOSE --output_wrapper %wrapper%
      
  EXIT /B 0

rem Credit to https://stackoverflow.com/questions/23075953/batch-script-to-find-and-replace-a-string-in-text-file-without-creating-an-extra
rem It's not the most efficient thing ever, but it gets the job done.  The shell script's version is WAY faster due to available tools.
:replace_in_file
@echo off 
  setlocal enableextensions disabledelayedexpansion

  set "search=%1"
  set "replace=%2"

  set "textFile=%3"

  for /f "delims=" %%i in ('type "%textFile%" ^& break ^> "%textFile%" ') do (
      set "line=%%i"
      setlocal enabledelayedexpansion
      >>"%textFile%" echo(!line:%search%=%replace%!
      endlocal
  )

  EXIT /B 0

:copy_resources
  echo 
  echo Copy resources to %1/ui, .../osk

  rem Create our entire compilation results path.  Can't one-line them due to shell-script parsing errors.

  rem Extended mkdir functionality - can recursively create directories as long as command extensions are enabled.
  rem They usually are, but... just in case.

  @echo off
  setlocal enableextensions
  if not exist %1 mkdir %1
  if not exist %1\ui mkdir %1\ui
  if not exist %1\osk mkdir %1\osk
  if not exist %1\src mkdir %1\src
  if not exist %1\src\ui mkdir %1\src\ui
  if not exist %1\src\osk mkdir %1\src\osk
  endlocal

  echo Copy resources to %1\ui, ...\osk
  xcopy %SOURCE%\resources\ui\* %1\ui /E /Y  >nul
  xcopy %SOURCE%\resources\osk\* %1\osk /E /Y  >nul

  echo Copy source to %1/src
  xcopy %SOURCE%\*.js %1\src /Y 
  xcopy %SOURCE%\*.ts %1\src /Y
  echo %BUILD% > %1\src\version.txt

  xcopy %SOURCE%\resources\ui\* %1\src\ui /E /Y  >nul
  xcopy %SOURCE%\resources\osk\* %1\src\osk /E /Y  >nul

  rem Update build number if successful
  echo
  echo KeymanWeb resources saved under %1
  echo
  EXIT /B 0

:main

if "%1" == "-ui"   goto ui
if "%1" == "-test" goto web
if "%1" == "-embed" goto embed
if "%1" == "-web"  goto web
if "%1" == "-debug_embedded" goto debug_embedded
if not "%1" == "" goto help


echo Compiling build %BUILD%
echo.

rem
rem Check KeymanWeb dependencies before build
rem

:embed

echo Compile KMEI/KMEA build %BUILD%

rem Extended mkdir functionality - can recursively create directories as long as command extensions are enabled.
rem They usually are, but... just in case.
@echo off
setlocal enableextensions
if not exist %EMBED_OUTPUT% mkdir %EMBED_OUTPUT%
if not exist %EMBED_OUTPUT%\resources mkdir %EMBED_OUTPUT%\resources
endlocal

del %EMBED_OUTPUT%\keyman.js 2>nul
CALL %compilecmd% -p %NODE_SOURCE%\tsconfig.embedded.json
echo Typescript compiled.

CALL :minify "keyman.js" %EMBED_OUTPUT% SIMPLE_OPTIMIZATIONS "keyman.__BUILD__=%BUILD%"
if not exist %EMBED_OUTPUT%\keyman.js goto fail

echo Compiled embedded application saved as keyman.js

rem Update any changed resources

echo Copy or update resources
xcopy %SOURCE%\resources\*.* %EMBED_OUTPUT%\resources /E /Y  >nul

rem Update build number if successful
echo.
echo KMEA/KMEI build %BUILD% compiled and saved.
echo.

if "%1" == "-embed" goto done

:web

rem Compile KeymanWeb code modules for native keymanweb use, stubbing out and removing references to debug functions
echo Compile Keymanweb...   

del %WEB_OUTPUT%\keymanweb.js 2>nul
CALL %compilecmd% -p %NODE_SOURCE%\tsconfig.web.json || true
echo Typescript compiled.

call :copy_resources %INTERMEDIATE%

echo Minifying KeymanWeb...
CALL :minify "keymanweb.js" %WEB_OUTPUT% SIMPLE_OPTIMIZATIONS "keyman.__BUILD__=%BUILD%"
if not exist %WEB_OUTPUT%\keymanweb.js goto fail

if "%1" == "-test" goto done

rem Update any changed resources

call :copy_resources %WEB_OUTPUT%

rem Update build number if successful
echo.
echo KeymanWeb 2 build %BUILD% compiled and saved.
echo.
rem echo %BUILD% >version.txt

rem exit /B 0
rem Compile UI code modules (TODO: add date testing, only recompile if needed)

:ui

echo Compile UI Modules...
CALL %compilecmd% -p %NODE_SOURCE%\tsconfig.ui.json

echo Minify ToolBar UI
del %WEB_OUTPUT%\kmuitoolbar.js 2>nul
CALL :minify "kmwuitoolbar.js" %WEB_OUTPUT% ADVANCED_OPTIMIZATIONS "" "(function() {%%%%output%%%%}());"
if not exist %WEB_OUTPUT%\kmwuitoolbar.js goto fail

echo Minify Toggle UI
del %WEB_OUTPUT%\kmuitoggle.js 2>nul
CALL :minify "kmwuitoggle.js" %WEB_OUTPUT% SIMPLE_OPTIMIZATIONS "" "(function() {%%%%output%%%%}());"
if not exist %WEB_OUTPUT%\kmwuitoggle.js goto fail

echo Minify Float UI
del %WEB_OUTPUT%\kmuifloat.js 2>nul
CALL :minify "kmwuifloat.js" %WEB_OUTPUT% ADVANCED_OPTIMIZATIONS "" "(function() {%%%%output%%%%}());"
if not exist %WEB_OUTPUT%\kmwuifloat.js goto fail

echo Minify Button UI
del %WEB_OUTPUT%\kmuibutton.js 2>nul
CALL :minify "kmwuibutton.js" %WEB_OUTPUT% ADVANCED_OPTIMIZATIONS "" "(function() {%%%%output%%%%}());"
if not exist %WEB_OUTPUT%\kmwuibutton.js goto fail

echo User interface modules compiled and saved.
exit /B 0

:fail
echo.
echo Build failed
exit /B 2

:debug_embedded
copy %INTERMEDIATE%\keyman.js %EMBED_OUTPUT%\keymanios.js

copy %INTERMEDIATE%\keyman.js.map %EMBED_OUTPUT%\keymanios.js.map

rem Problem - must correct the resulting map location!
echo Correcting source map link.  This may take a while; using the shell script or manually correcting the link will be faster.
call :replace_in_file keyman.js.map keymanios.js.map %EMBED_OUTPUT%\keymanios.js

echo Uncompiled embedded application saved as keymanios.js

goto done

:help
echo.
echo Usage: build                   to compile keymanweb application code to output folder
echo        build -ui               to compile desktop user interface modules to output folder
echo        build -test             to compile for testing without copying resources or
echo                                updating the saved version number.
echo        build -debug_embedded   to compile a readable version of the embedded KMEA/KMEI code.
echo        build -web              to compile only the KeymanWeb engine.
echo        build -embed            to compile only the KMEA/KMEI embedded engine.
exit /B 1

:done
echo.
exit /B 0

if "%1" == "-ui"   goto ui
if "%1" == "-test" goto web
if "%1" == "-embed" goto embed
if "%1" == "-web"  goto web
if "%1" == "-debug_embedded" goto debug_embedded
if not "%1" == "" goto help
