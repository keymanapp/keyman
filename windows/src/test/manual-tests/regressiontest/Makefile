#
# REGRESSIONTEST Makefile
#


!include ..\..\Defines.mak

build: dirs 
    $(DCC32) regressiontest.dpr
    $(COPY) regressiontest.exe $(PROGRAM)\support

clean: def-clean
    cd tests
    $(MAKE) clean

!include ..\..\Target.mak
