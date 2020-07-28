#
# TSysInfox64 Makefile
#

!include ..\..\Defines.mak

build: version.res manifest.res dirs
    $(DELPHI_MSBUILD) tsysinfox64.dproj /p:Platform=Win64
    $(SENTRYTOOL_DELPHIPREP) $(WIN64_TARGET_PATH)\tsysinfox64.exe -dpr tsysinfox64.dpr
    $(COPY) $(WIN64_TARGET_PATH)\tsysinfox64.exe $(PROGRAM)\engine

clean: def-clean

signcode:
    $(SIGNCODE) /d "Keyman Engine x64 Diagnostics" $(PROGRAM)\engine\tsysinfox64.exe

backup:
    $(WZZIP) $(BUILD)\engine\tsysinfox64.zip  $(BACKUPDEFAULTS) $(WIN64_TARGET_PATH)\*.map $(WIN64_TARGET_PATH)\tsysinfox64.exe

install:
    echo Nothing to do.

test-manifest:
    # test that linked manifest exists and correct
    $(MT) -nologo -inputresource:$(PROGRAM)\engine\tsysinfox64.exe -validate_manifest

!include ..\..\Target.mak
