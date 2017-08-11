#
# Embeddedwb Makefile (Tavultesoft)
#

!include ..\..\Defines.mak

build: dirs
    cd Source
    $(DCC32DPK) ..\Packages\EmbeddedWebBrowser_XE.dpk
    cd ..
    $(MAKE) install

install:
    $(DEVTOOLS) -ip $(OUTLIB)\EmbeddedWebBrowser_XE.bpl
    $(DEVTOOLS) -ai $(EXT)\embeddedwb\source

clean: def-clean

!include ..\..\Target.mak
