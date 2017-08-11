@ECHO OFF
REM  Licensed to the Apache Software Foundation (ASF) under one or more
REM  contributor license agreements.  See the NOTICE file distributed with
REM  this work for additional information regarding copyright ownership.
REM  The ASF licenses this file to You under the Apache License, Version 2.0
REM  (the "License"); you may not use this file except in compliance with
REM  the License.  You may obtain a copy of the License at
REM
REM      http://www.apache.org/licenses/LICENSE-2.0
REM
REM  Unless required by applicable law or agreed to in writing, software
REM  distributed under the License is distributed on an "AS IS" BASIS,
REM  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
REM  See the License for the specific language governing permissions and
REM  limitations under the License.

rem %~dp0 is the expanded pathname of the current script under NT
set LOCAL_FOP_HOME=
if "%OS%"=="Windows_NT" set LOCAL_FOP_HOME=%~dp0

rem Code from Apache Ant project
rem Slurp the command line arguments. This loop allows for an unlimited number
rem of arguments (up to the command line limit, anyway).
rem Could also do a "shift" and "%*" for all params, but apparently doesn't work 
rem with Win9x.
set FOP_CMD_LINE_ARGS=%1
if ""%1""=="""" goto doneStart
shift
:setupArgs
if ""%1""=="""" goto doneStart
set FOP_CMD_LINE_ARGS=%FOP_CMD_LINE_ARGS% %1
shift
goto setupArgs
rem This label provides a place for the argument list loop to break out 
rem and for NT handling to skip to.
:doneStart

set LOGCHOICE=
rem The default commons logger for JDK1.4 is JDK1.4Logger.
rem To use a different logger, uncomment the one desired below
rem set LOGCHOICE=-Dorg.apache.commons.logging.Log=org.apache.commons.logging.impl.NoOpLog
rem set LOGCHOICE=-Dorg.apache.commons.logging.Log=org.apache.commons.logging.impl.SimpleLog
rem set LOGCHOICE=-Dorg.apache.commons.logging.Log=org.apache.commons.logging.impl.Log4JLogger

set LOGLEVEL=
rem Logging levels
rem Below option is only if you are using SimpleLog instead of the default JDK1.4 Logger.
rem To set logging levels for JDK 1.4 Logger, edit the %JAVA_HOME%\JRE\LIB\logging.properties 
rem file instead.
rem Possible SimpleLog values:  "trace", "debug", "info" (default), "warn", "error", or "fatal".
rem set LOGLEVEL=-Dorg.apache.commons.logging.simplelog.defaultlog=INFO

set LIBDIR=%LOCAL_FOP_HOME%lib

set LOCALCLASSPATH=%LOCAL_FOP_HOME%build\fop.jar
set LOCALCLASSPATH=%LOCALCLASSPATH%;%LOCAL_FOP_HOME%build\fop-sandbox.jar
set LOCALCLASSPATH=%LOCALCLASSPATH%;%LOCAL_FOP_HOME%build\fop-hyph.jar
set LOCALCLASSPATH=%LOCALCLASSPATH%;%LIBDIR%\xml-apis-1.3.04.jar
set LOCALCLASSPATH=%LOCALCLASSPATH%;%LIBDIR%\xml-apis-ext-1.3.04.jar
set LOCALCLASSPATH=%LOCALCLASSPATH%;%LIBDIR%\xercesImpl-2.7.1.jar
set LOCALCLASSPATH=%LOCALCLASSPATH%;%LIBDIR%\xalan-2.7.0.jar
set LOCALCLASSPATH=%LOCALCLASSPATH%;%LIBDIR%\serializer-2.7.0.jar
set LOCALCLASSPATH=%LOCALCLASSPATH%;%LIBDIR%\batik-all-1.7.jar
set LOCALCLASSPATH=%LOCALCLASSPATH%;%LIBDIR%\xmlgraphics-commons-1.3.1.jar
set LOCALCLASSPATH=%LOCALCLASSPATH%;%LIBDIR%\avalon-framework-4.2.0.jar
set LOCALCLASSPATH=%LOCALCLASSPATH%;%LIBDIR%\commons-io-1.3.1.jar
set LOCALCLASSPATH=%LOCALCLASSPATH%;%LIBDIR%\commons-logging-1.0.4.jar
set LOCALCLASSPATH=%LOCALCLASSPATH%;%LIBDIR%\jai_imageio.jar
set LOCALCLASSPATH=%LOCALCLASSPATH%;%LIBDIR%\fop-hyph.jar
set LOCALCLASSPATH=%LOCALCLASSPATH%;%FOP_HYPHENATION_PATH%

set JAVAOPTS=-Denv.windir=%WINDIR%

if "%JAVA_HOME%" == "" goto noJavaHome
if not exist "%JAVA_HOME%\bin\java.exe" goto noJavaHome
if "%JAVACMD%" == "" set JAVACMD=%JAVA_HOME%\bin\java
goto runFop

:noJavaHome
if "%JAVACMD%" == "" set JAVACMD=java

:runFop
rem ECHO "%JAVACMD%"
"%JAVACMD%" %JAVAOPTS% %LOGCHOICE% %LOGLEVEL% -cp "%LOCALCLASSPATH%" org.apache.fop.cli.Main %FOP_CMD_LINE_ARGS%
