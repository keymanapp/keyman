#
# test for signatures and version information being correct in ?install directory or ?bin directory
#

!include ..\..\Defines.mak

build:
    $(DELPHI_MSBUILD) verify_signatures.dproj
    copy sigcheck.bin $(WIN32_TARGET_PATH)\sigcheck.exe

clean: def-clean

!include ..\..\Target.mak
