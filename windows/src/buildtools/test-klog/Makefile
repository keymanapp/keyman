#
# Verify klog is disabled
#

!include ..\..\Defines.mak

verify: clean
    $(DCC32) test_klog.dpr
    test_klog

build:
    rem no build for test_certcheck

clean: def-clean

!include ..\..\Target.mak
  