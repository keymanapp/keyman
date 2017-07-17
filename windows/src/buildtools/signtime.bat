@echo off    

set SERVERLIST=(http://timestamp.comodoca.com/authenticode http://timestamp.verisign.com/scripts/timstamp.dll http://timestamp.globalsign.com/scripts/timestamp.dll http://tsa.starfieldtech.com)
set RFC3161SERVERLIST=(http://timestamp.comodoca.com/rfc3161)
set SIGNTOOL=%1
set PFX_SHA1=%2
set PFX_SHA256=%3
set URL=%4
set PWD=%5

set SIGNFILE=%6
set SIGNNAME=Keyman

if "%SIGNFILE%" == "/d" (
  set SIGNNAME=%7
  set SIGNFILE=%8
) 

set timestampErrors=0

for %%a in (%SIGNFILE%) do if "%%~xa"==".msi" set ISMSI=1
for %%a in (%SIGNFILE%) do if "%%~xa"==".msm" set ISMSI=1

if "%ISMSI%" == "1" goto sign-msi

:sha1
%SIGNTOOL% sign /f %PFX_SHA1% /fd sha1 /du %URL% /p %PWD% /v /d %SIGNNAME% %SIGNFILE%
if ERRORLEVEL 0 if not ERRORLEVEL 1 goto timestamp-sha1
goto fail

:sha256
%SIGNTOOL% sign /as /f %PFX_SHA256% /fd sha256 /du %URL% /p %PWD% /v /d %SIGNNAME% %SIGNFILE%
if ERRORLEVEL 0 if not ERRORLEVEL 1 goto timestamp-sha256
goto fail

:sign-msi
rem msi files cannot be dual signed
%SIGNTOOL% sign /f %PFX_SHA256% /fd sha256 /du %URL% /p %PWD% /v /d %SIGNNAME% %SIGNFILE%
if ERRORLEVEL 0 if not ERRORLEVEL 1 goto timestamp-msi
goto fail


:timestamp-sha1

for /L %%a in (1,1,10) do (

    for %%s in %SERVERLIST% do (

        echo Trying to sign %SIGNFILE% with %%s

        REM try to timestamp the file. This operation is unreliable and may need to be repeated...
        %SIGNTOOL% timestamp /t %%s /v %SIGNFILE%

        REM check the return value of the timestamping operation and retry a max of ten times...
        if ERRORLEVEL 0 if not ERRORLEVEL 1 GOTO sha256

        echo Signing failed. Probably cannot find the timestamp server at %%s
        set /a timestampErrors+=1
    )

    REM wait 2 seconds...
    choice /N /T:2 /D:Y >NUL
)

:timestamp-sha256

for /L %%a in (1,1,10) do (

    for %%s in %RFC3161SERVERLIST% do (

        echo Trying to sign %SIGNFILE% with %%s

        REM try to timestamp the file. This operation is unreliable and may need to be repeated...
        rem %SIGNTOOL% timestamp /tr %%s /td sha256 /v %SIGNFILE%
        rem if dual signing with sha1, use the line below instead:
        %SIGNTOOL% timestamp /tr %%s /tp 1 /td sha256 /v %SIGNFILE%

        REM check the return value of the timestamping operation and retry a max of ten times...
        if ERRORLEVEL 0 if not ERRORLEVEL 1 GOTO succeeded

        echo Signing failed. Probably cannot find the timestamp server at %%s
        set /a timestampErrors+=1
    )

    REM wait 2 seconds...
    choice /N /T:2 /D:Y >NUL
)


:timestamp-msi

for /L %%a in (1,1,10) do (

    for %%s in %RFC3161SERVERLIST% do (

        echo Trying to sign %SIGNFILE% with %%s

        REM try to timestamp the file. This operation is unreliable and may need to be repeated...
        %SIGNTOOL% timestamp /tr %%s /td sha256 /v %SIGNFILE%

        REM check the return value of the timestamping operation and retry a max of ten times...
        if ERRORLEVEL 0 if not ERRORLEVEL 1 GOTO succeeded

        echo Signing failed. Probably cannot find the timestamp server at %%s
        set /a timestampErrors+=1
    )

    REM wait 2 seconds...
    choice /N /T:2 /D:Y >NUL
)


:fail
REM return an error code...
echo signtime.bat exit code is 1. There were %timestampErrors% timestamping errors.
exit /b 1

:succeeded
REM return a successful code...
echo signtime.bat exit code is 0. There were %timestampErrors% timestamping errors.
exit /b 0
