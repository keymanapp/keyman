@echo off
rem 
rem Compile keymanweb and copy compiled javascript and resources to output/embedded folder
rem

rem Definition of global compile constants
set WEB_OUTPUT="..\output"
set EMBED_OUTPUT="..\embedded"
set SOURCE="."

rem Get build version -- if not building in TeamCity, then always use 300

if "%BUILD_COUNTER%"=="" (
  set BUILD=300
) else (
  set BUILD=%BUILD_COUNTER%
)

if "%CLOSURECOMPILERPATH%"=="" set CLOSURECOMPILERPATH=..\tools
if "%JAVA%"=="" set JAVA=java

set compiler=%CLOSURECOMPILERPATH%\compiler.jar
set compilecmd="%JAVA%" -jar "%compiler%"

if not exist %compiler% (
  echo File %compiler% does not exist: have you set the environment variable CLOSURECOMPILERPATH?
  exit /B 1
)

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

rem Compile supplementary plane string handing extensions
echo Compile SMP string extensions
del %EMBED_OUTPUT%\kmw-smpstring.js 2>nul
%compilecmd% --js %SOURCE%\kmwstring.js --compilation_level SIMPLE_OPTIMIZATIONS --js_output_file %EMBED_OUTPUT%\kmw-smpstring.js --warning_level VERBOSE
if not exist %EMBED_OUTPUT%\kmw-smpstring.js goto fail

del kmwtemp.js 2>nul
%compilecmd% --define __BUILD__=%BUILD% --externs %SOURCE%\kmwreleasestub.js --js %SOURCE%\kmwbase.js --js %SOURCE%\keymanweb.js --js %SOURCE%\kmwosk.js --js %SOURCE%\kmwembedded.js --js %SOURCE%\kmwcallback.js --js %SOURCE%\kmwkeymaps.js --js %SOURCE%\kmwlayout.js --js %SOURCE%\kmwinit.js --compilation_level SIMPLE_OPTIMIZATIONS  --js_output_file kmwtemp.js --warning_level VERBOSE
if not exist kmwtemp.js goto fail

echo Append SMP extensions
copy /B %EMBED_OUTPUT%\kmw-smpstring.js+kmwtemp.js %EMBED_OUTPUT%\keyman.js >nul
del kmwtemp.js

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

rem Extended mkdir functionality - can recursively create directories as long as command extensions are enabled.
rem They usually are, but... just in case.
@echo off
setlocal enableextensions
if not exist %WEB_OUTPUT% mkdir %WEB_OUTPUT%
if not exist %WEB_OUTPUT%\ui mkdir %WEB_OUTPUT%\ui
if not exist %WEB_OUTPUT%\osk mkdir %WEB_OUTPUT%\osk
if not exist %WEB_OUTPUT%\src mkdir %WEB_OUTPUT%\src
if not exist %WEB_OUTPUT%\src\ui mkdir %WEB_OUTPUT%\src\ui
if not exist %WEB_OUTPUT%\src\osk mkdir %WEB_OUTPUT%\src\osk
endlocal

rem Compile supplementary plane string handing extensions
echo Compile SMP string extensions
del %WEB_OUTPUT%\kmw-smpstring.js 2>nul
%compilecmd% --js %SOURCE%\kmwstring.js --compilation_level SIMPLE_OPTIMIZATIONS --js_output_file %WEB_OUTPUT%\kmw-smpstring.js --warning_level VERBOSE
if not exist %WEB_OUTPUT%\kmw-smpstring.js goto fail

rem Compile KeymanWeb code modules for native keymanweb use, stubbing out and removing references to debug functions
echo Compile Keymanweb    

del %WEB_OUTPUT%\kmwtemp.js 2>nul
%compilecmd% --define __BUILD__=%BUILD% --externs %SOURCE%\kmwreleasestub.js --js %SOURCE%\kmwbase.js --js %SOURCE%\keymanweb.js --js %SOURCE%\kmwosk.js --js %SOURCE%\kmwnative.js --js %SOURCE%\kmwcallback.js --js %SOURCE%\kmwkeymaps.js --js %SOURCE%\kmwlayout.js --js %SOURCE%\kmwinit.js --compilation_level SIMPLE_OPTIMIZATIONS  --js_output_file %WEB_OUTPUT%\kmwtemp.js --warning_level VERBOSE
if not exist %WEB_OUTPUT%\kmwtemp.js goto fail

echo Append SMP string extensions to Keymanweb
copy /B %WEB_OUTPUT%\kmw-smpstring.js+%WEB_OUTPUT%\kmwtemp.js %WEB_OUTPUT%\keymanweb.js
del %WEB_OUTPUT%\kmwtemp.js

if "%1" == "-test" goto done

rem Update any changed resources

echo Copy resources to %WEB_OUTPUT%\ui, ...\osk
xcopy %SOURCE%\resources\ui\* %WEB_OUTPUT%\ui /E /Y  >nul
xcopy %SOURCE%\resources\osk\* %WEB_OUTPUT%\osk /E /Y  >nul

echo Copy source to %WEB_OUTPUT%\src
xcopy %SOURCE%\*.js %WEB_OUTPUT%\src /Y 
echo %BUILD% > %WEB_OUTPUT%\src\version.txt
xcopy %SOURCE%\resources\ui\* %WEB_OUTPUT%\src\ui /E /Y  >nul
xcopy %SOURCE%\resources\osk\* %WEB_OUTPUT%\src\osk /E /Y  >nul

rem Update build number if successful
echo.
echo KeymanWeb 2 build %BUILD% compiled and saved.
echo.
rem echo %BUILD% >version.txt

rem exit /B 0
rem Compile UI code modules (TODO: add date testing, only recompile if needed)

:ui

echo Compile ToolBar UI
del %WEB_OUTPUT%\kmuitoolbar.js 2>nul
%compilecmd% --js %SOURCE%\kmwuitoolbar.js --externs %SOURCE%\kmwreleasestub.js --compilation_level ADVANCED_OPTIMIZATIONS --js_output_file %WEB_OUTPUT%\kmwuitoolbar.js --warning_level VERBOSE --output_wrapper "(function() {%%output%%}());"
if not exist %WEB_OUTPUT%\kmwuitoolbar.js goto fail

echo Compile Toggle UI
del %WEB_OUTPUT%\kmuitoggle.js 2>nul
%compilecmd% --js %SOURCE%\kmwuitoggle.js --externs %SOURCE%\kmwreleasestub.js --compilation_level SIMPLE_OPTIMIZATIONS --js_output_file %WEB_OUTPUT%\kmwuitoggle.js --warning_level VERBOSE --output_wrapper "(function() {%%output%%}());"
if not exist %WEB_OUTPUT%\kmwuitoggle.js goto fail

echo Compile Float UI
del %WEB_OUTPUT%\kmuifloat.js 2>nul
%compilecmd% --js %SOURCE%\kmwuifloat.js --externs %SOURCE%\kmwreleasestub.js --compilation_level ADVANCED_OPTIMIZATIONS --js_output_file %WEB_OUTPUT%\kmwuifloat.js --warning_level VERBOSE --output_wrapper "(function() {%%output%%}());"
if not exist %WEB_OUTPUT%\kmwuifloat.js goto fail

echo Compile Button UI
del %WEB_OUTPUT%\kmuibutton.js 2>nul
%compilecmd% --js %SOURCE%\kmwuibutton.js --externs %SOURCE%\kmwreleasestub.js --compilation_level SIMPLE_OPTIMIZATIONS --js_output_file %WEB_OUTPUT%\kmwuibutton.js --warning_level VERBOSE --output_wrapper "(function() {%%output%%}());"
if not exist %WEB_OUTPUT%\kmwuibutton.js goto fail

echo User interface modules compiled and saved.
exit /B 0

:fail
echo.
echo Build failed
exit /B 2

:debug_embedded
copy /B /Y %SOURCE%\kmwstring.js+%SOURCE%\kmwbase.js+%SOURCE%\keymanweb.js+%SOURCE%\kmwcallback.js+%SOURCE%\kmwosk.js+kmwembedded.js+%SOURCE%\kmwkeymaps.js+%SOURCE%\kmwlayout.js+%SOURCE%\kmwinit.js %EMBED_OUTPUT%\keymanios.js
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
