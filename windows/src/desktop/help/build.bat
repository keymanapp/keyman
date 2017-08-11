@echo off
rem #-------------------------------------------------------
rem # HTML build
rem #-------------------------------------------------------

if "%1"=="-?" goto help

if exist ..\..\..\bin\help\desktop rd /s/q ..\..\..\bin\help\desktop
mkdir ..\..\..\bin
mkdir ..\..\..\bin\help\desktop
mkdir ..\..\..\bin\help\desktop
mkdir ..\..\..\bin\help\desktop\desktop_images
mkdir ..\..\..\bin\help\desktop\chm

xcopy /s/q desktop_images\* ..\..\..\bin\help\desktop\desktop_images
copy topicresponse.js ..\..\..\bin\help\desktop\
copy kmhelp.css ..\..\..\bin\help\desktop\
copy topicresponse.js ..\..\..\bin\help\desktop\chm\
copy kmhelp.css ..\..\..\bin\help\desktop\chm\

if "%1"=="html" goto html
if "%1"=="chm" goto chm

:html

..\..\ext\libxslt\xsltproc --xinclude --timing html.xsl index.xml

goto end

:chm

if not exist "%programfiles%\HTML Help Workshop\hhc.exe" goto chmerror

cd source
..\..\ext\libxslt\xsltproc --xinclude --timing chm.xsl index.xml
"%programfiles%\HTML Help Workshop\hhc.exe" keymandesktop.hhp
move keymandesktop.hhp ..\..\..\bin\help\desktop\chm
move toc.hhc ..\..\..\bin\help\desktop\chm
move keymandesktop.chm ..\..\..\bin\help\desktop\chm

goto end

:chmerror

echo Could not find the HTML Help Workshop compiler.
echo.
echo.

:help

echo Usage: %0 chm/html
echo.  %0 chm   will create a HTML Help .chm file
echo.  %0 html  will create a set of HTML files for viewing in your web browser, for review purposes
echo.
echo.  To build the .chm file, you will need to have the HTML Help Workshop installed on your system in
echo.    %programfiles%\HTML Help Workshop
echo.  The HTML Help Workshop is a free download from microsoft.com

:end
