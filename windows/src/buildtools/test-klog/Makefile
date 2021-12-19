#
# Verify klog is disabled
#

!include ..\..\Defines.mak

verify: clean
    $(DELPHI_MSBUILD) test_klog.dproj "/p:Platform=Win32"
    $(WIN32_TARGET_PATH)\test_klog

build:
    rem no build for test_certcheck

clean: def-clean

!include ..\..\Target.mak
