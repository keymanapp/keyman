@echo off
rem 
rem Compile keymanweb and copy compiled javascript and resources to output folder
rem

rem Get build version -- if not building in TeamCity, then always use 300

if "%BUILD_NUMBER%"=="" (
  set BUILD=300
) else (
  set BUILD=%BUILD_NUMBER%
)

if "%1" == "-ui"   goto ui
if "%1" == "-test" goto web
if "%1" == "-embed" goto embed
if "%1" == "-web"  goto web
if "%1" == "-debug_embedded" goto debug_embedded
if not "%1" == "" goto help

:embed

echo Compile KMEI/KMEA build %BUILD%

if not exist ..\embedded mkdir ..\embedded
if not exist ..\embedded\resources mkdir ..\embedded\resources

rem Compile supplementary plane string handing extensions
echo Compile SMP string extensions
del ..\embedded\kmw-smpstring.js 2>nul
%compilecmd% --js kmwstring.js --compilation_level SIMPLE_OPTIMIZATIONS --js_output_file ..\embedded\kmw-smpstring.js --warning_level VERBOSE
if not exist ..\embedded\kmw-smpstring.js goto fail

del kmwtemp.js 2>nul
java -jar ..\tools\compiler.jar --define __BUILD__=%BUILD% --externs ..\source\kmwreleasestub.js --js ..\source\kmwbase.js --js ..\source\keymanweb.js --js ..\source\kmwosk.js --js kmwembedded.js --js ..\source\kmwcallback.js --js ..\source\kmwkeymaps.js --js ..\source\kmwlayout.js --js ..\source\kmwinit.js --compilation_level SIMPLE_OPTIMIZATIONS  --js_output_file kmwtemp.js --warning_level VERBOSE
if not exist kmwtemp.js goto fail

echo Append SMP extensions
copy /B ..\embedded\kmw-smpstring.js+kmwtemp.js ..\embedded\keymanweb-%BUILD%.js >nul
del kmwtemp.js

echo Compiled embedded application saved as keymanweb-%BUILD%.js

rem Update any changed resources

echo Copy or update resources
xcopy ..\source\resources\*.* ..\embedded\resources /D /E /Y  >nul

if "%1" == "-embed" goto done

:web

rem
rem Check KeymanWeb dependencies before build
rem

if "%CLOSURECOMPILERPATH%"=="" set CLOSURECOMPILERPATH=..\tools
if "%JAVA%"=="" set JAVA=java

set compiler=%CLOSURECOMPILERPATH%\compiler.jar
set compilecmd="%JAVA%" -jar "%compiler%"

if not exist %compiler% (
  echo File %compiler% does not exist: have you set the environment variable CLOSURECOMPILERPATH?
  exit /B 1
)

echo Compiling build %BUILD%
echo.

if not exist ..\output mkdir ..\output
if not exist ..\output\resources mkdir ..\output\resources

rem Compile supplementary plane string handing extensions
echo Compile SMP string extensions
del ..\output\kmw-smpstring.js 2>nul
%compilecmd% --js kmwstring.js --compilation_level SIMPLE_OPTIMIZATIONS --js_output_file ..\output\kmw-smpstring.js --warning_level VERBOSE
if not exist ..\output\kmw-smpstring.js goto fail

rem Compile KeymanWeb code modules for native keymanweb use, stubbing out and removing references to debug functions
echo Compile Keymanweb    

del ..\output\kmwtemp.js 2>nul
%compilecmd% --define __BUILD__=%BUILD% --externs kmwreleasestub.js --js kmwbase.js --js keymanweb.js --js kmwosk.js --js kmwnative.js --js kmwcallback.js --js kmwkeymaps.js --js kmwlayout.js --js kmwinit.js --compilation_level SIMPLE_OPTIMIZATIONS  --js_output_file ..\output\kmwtemp.js --warning_level VERBOSE
if not exist ..\output\kmwtemp.js goto fail

echo Append SMP string extensions to Keymanweb
copy /B ..\output\kmw-smpstring.js+..\output\kmwtemp.js ..\output\keymanweb.js
del ..\output\kmwtemp.js

if "%1" == "-test" goto done

rem Update any changed resources

echo Copy or update resources
xcopy resources\*.* ..\output\resources /D /E /Y  >nul

rem Update build number if successful
echo.
echo KeymanWeb 2 build %BUILD% compiled and saved.
echo.
rem echo %BUILD% >version.txt

rem exit /B 0
rem Compile UI code modules (TODO: add date testing, only recompile if needed)

:ui

echo Compile ToolBar UI
del ..\output\kmuitoolbar.js 2>nul
%compilecmd% --js kmwuitoolbar.js --externs kmwreleasestub.js --compilation_level ADVANCED_OPTIMIZATIONS --js_output_file ..\output\kmwuitoolbar.js --warning_level VERBOSE --output_wrapper "(function() {%%output%%}());"
if not exist ..\output\kmwuitoolbar.js goto fail

echo Compile Toggle UI
del ..\output\kmuitoggle.js 2>nul
%compilecmd% --js kmwuitoggle.js --externs kmwreleasestub.js --compilation_level SIMPLE_OPTIMIZATIONS --js_output_file ..\output\kmwuitoggle.js --warning_level VERBOSE --output_wrapper "(function() {%%output%%}());"
if not exist ..\output\kmwuitoggle.js goto fail

echo Compile Float UI
del ..\output\kmuifloat.js 2>nul
%compilecmd% --js kmwuifloat.js --externs kmwreleasestub.js --compilation_level ADVANCED_OPTIMIZATIONS --js_output_file ..\output\kmwuifloat.js --warning_level VERBOSE --output_wrapper "(function() {%%output%%}());"
if not exist ..\output\kmwuifloat.js goto fail

echo Compile Button UI
del ..\output\kmuibutton.js 2>nul
%compilecmd% --js kmwuibutton.js --externs kmwreleasestub.js --compilation_level SIMPLE_OPTIMIZATIONS --js_output_file ..\output\kmwuibutton.js --warning_level VERBOSE --output_wrapper "(function() {%%output%%}());"
if not exist ..\output\kmwuibutton.js goto fail

echo User interface modules compiled and saved.
exit /B 0

:fail
echo.
echo Build failed
exit /B 2

:debug_embedded
copy /B /Y ..\source\kmwstring.js+..\source\kmwbase.js+..\source\keymanweb.js+..\source\kmwcallback.js+..\source\kmwosk.js+kmwembedded.js+..\source\kmwkeymaps.js+..\source\kmwlayout.js+..\source\kmwinit.js keymanios.js
echo Uncompiled embedded application saved as keymanios.js

goto done

:help
echo.
echo Usage: build       to compile keymanweb application code to output folder
echo        build -ui   to compile desktop user interface modules to output folder
echo        build -test to compile for testing without copying resources or
echo                    updating the saved version number.
exit /B 1

:done
echo.
exit /B 0
