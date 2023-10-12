#
# SentryClientTest Makefile
#

!include ..\..\..\Defines.mak

build: dirs
# version.res manifest.res
    $(DELPHI_MSBUILD) SentryClientTest.dproj "/p:Platform=Win32"
    $(DELPHI_MSBUILD) SentryClientVclTest.dproj "/p:Platform=Win32"

    $(SENTRYTOOL_DELPHIPREP) $(WIN32_TARGET_PATH)\SentryClientTest.exe -dpr SentryClientTest.dpr
    $(SENTRYTOOL_DELPHIPREP) $(WIN32_TARGET_PATH)\SentryClientVclTest.exe -dpr SentryClientVclTest.dpr
    $(TDS2DBG) $(WIN32_TARGET_PATH)\SentryClientTest.exe
    $(TDS2DBG) $(WIN32_TARGET_PATH)\SentryClientVclTest.exe

upload-symbols:
    sentry-cli upload-dif -p keyman-windows --wait --include-sources .

clean: def-clean
    -rd /s/q Win32

signcode:
    rem no action

!include ..\..\..\Target.mak
