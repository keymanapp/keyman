#
# Keyman Developer installation builder Makefile
#

!include ..\..\Defines.mak

# ----------------------------------------------------------------------

DEVELOPER_FILES=kmdev.wixobj xml.wixobj cef.wixobj templates.wixobj kmlmc.wixobj

setup:

    #
    # Prepare file version information
    #

    cd $(ROOT)\src\developer\inst
    $(PROGRAM)\buildtools\mkver -v -u copydev.in copydev.mak $(ROOT)\src\developer\version.txt
    
    #
    # Build the installation archive
    #
    
    $(MAKE) -fcopydev.mak candle
    $(WIXLIGHT) -sice:ICE91 -sice:ICE60 -dWixUILicenseRtf=License.rtf -out keymandeveloper.msi -ext WixUIExtension $(DEVELOPER_FILES)

    #
    # Sign the installation archive
    #

    $(SIGNCODE) /d "Keyman Developer" keymandeveloper.msi

    #
    # Copy the installation archive
    #

    cd $(ROOT)\src\developer\inst
    $(MAKE) -fcopydev.mak

build:
    @rem
	
signcode:
    @rem

backup:
    @rem
	
clean:
    cd $(ROOT)\src\developer\inst
    -del /Q copydev.mak
    -del /Q *.msi
    -del /Q *.msp
    -del /Q *.wixobj
    -del /Q *.log
    -del /Q *.pcp
    -del /Q setup.inf
    -del /Q setup.zip
    -del /Q *.exe
    -del /Q *.wixpdb
    -del /Q xml.wxs
    -del /Q cef.wxs
    -del /Q templates.wxs
    -del /Q kmlmc.wxs

test-releaseexists:
    cd $(ROOT)\src\developer\inst
    $(PROGRAM)\buildtools\mkver -v -u copydev.in copydev.mak $(ROOT)\src\version.txt

    $(MAKE) -fcopydev.mak test-releaseexists

install:
    @rem

# ----------------------------------------------------------------------

!include ..\..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
