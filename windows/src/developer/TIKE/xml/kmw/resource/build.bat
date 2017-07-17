@echo off
rem 
rem Compile keymanweb and copy compiled javascript and resources to output folder
rem

rem Get last build number

set BUILD=300
set /p BUILD=<version.txt

if "%1" == "-ui"   goto ui
if "%1" == "-test" goto web
if not "%1" == "" goto help

:web

rem Increment build number
set /a BUILD=%BUILD%+1
echo.
echo Compiling build %BUILD%
echo.

rem Compile supplementary plane string handing extensions
echo Compile SMP string extensions
del ..\output\kmw-smpstring.js 2>nul
java -jar ..\tools\compiler.jar --js kmwstring.js --compilation_level SIMPLE_OPTIMIZATIONS --js_output_file ..\output\kmw-smpstring.js --warning_level VERBOSE
if not exist ..\output\kmw-smpstring.js goto fail

rem Compile KeymanWeb code modules for native keymanweb use, stubbing out and removing references to debug functions
echo Compile Keymanweb    

del ..\output\kmwtemp.js 2>nul
java -jar ..\tools\compiler.jar --define __BUILD__=%BUILD% --externs kmwreleasestub.js --js kmwbase.js --js keymanweb.js --js kmwosk.js --js kmwnative.js --js kmwcallback.js --js kmwkeymaps.js --js kmwlayout.js --js kmwinit.js --compilation_level SIMPLE_OPTIMIZATIONS  --js_output_file ..\output\kmwtemp.js --warning_level VERBOSE
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
echo %BUILD% >version.txt

goto done

rem Compile UI code modules (TODO: add date testing, only recompile if needed)

:ui

echo Compile ToolBar UI
del ..\output\kmuitoolbar.js 2>nul
java -jar ..\tools\compiler.jar --js kmwuitoolbar.js --externs kmwreleasestub.js --compilation_level ADVANCED_OPTIMIZATIONS --js_output_file ..\output\kmwuitoolbar.js --warning_level VERBOSE --output_wrapper "(function() {%%output%%}());"
if not exist ..\output\kmwuitoolbar.js goto fail

echo Compile Toggle UI
del ..\output\kmuitoggle.js 2>nul
java -jar ..\tools\compiler.jar --js kmwuitoggle.js --externs kmwreleasestub.js --compilation_level SIMPLE_OPTIMIZATIONS --js_output_file ..\output\kmwuitoggle.js --warning_level VERBOSE --output_wrapper "(function() {%%output%%}());"
if not exist ..\output\kmwuitoggle.js goto fail

echo Compile Float UI
del ..\output\kmuifloat.js 2>nul
java -jar ..\tools\compiler.jar --js kmwuifloat.js --externs kmwreleasestub.js --compilation_level ADVANCED_OPTIMIZATIONS --js_output_file ..\output\kmwuifloat.js --warning_level VERBOSE --output_wrapper "(function() {%%output%%}());"
if not exist ..\output\kmwuifloat.js goto fail

echo Compile Button UI
del ..\output\kmuibutton.js 2>nul
java -jar ..\tools\compiler.jar --js kmwuibutton.js --externs kmwreleasestub.js --compilation_level SIMPLE_OPTIMIZATIONS --js_output_file ..\output\kmwuibutton.js --warning_level VERBOSE --output_wrapper "(function() {%%output%%}());"
if not exist ..\output\kmwuibutton.js goto fail

echo User interface modules compiled and saved.
goto done

:fail
echo.
echo Build failed
goto done

:help
echo.
echo Usage: build       to compile keymanweb application code to output folder
echo        build -ui   to compile desktop user interface modules to output folder
echo        build -test to compile for testing without copying resources or
echo                    updating the saved version number.

:done
echo.
