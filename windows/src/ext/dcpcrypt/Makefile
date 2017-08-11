#
# DcpCrypt Makefile (Tavultesoft)
#

!include ..\..\Defines.mak

build: dirs
    $(DCC32DPK) DCPdelphi2009.dpk
    $(MAKE) install

install:
    $(DEVTOOLS) -ip $(OUTLIB)\DCPdelphi2009.bpl
    $(DEVTOOLS) -ai $(EXT)\dcpcrypt
    $(DEVTOOLS) -ai $(EXT)\dcpcrypt\hashes
    $(DEVTOOLS) -ai $(EXT)\dcpcrypt\ciphers

clean: def-clean
    $(CLEAN) *.bpl
    $(CLEAN) *.dcp

!include ..\..\Target.mak
