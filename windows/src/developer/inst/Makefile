#
# Keyman Developer installation builder Makefile
#

!include ..\..\Defines.mak

# ----------------------------------------------------------------------

DEVELOPER_FILES=kmdev.wixobj xml.wixobj cef.wixobj templates.wixobj kmlmc.wixobj server.wixobj

setup: pull-server
#
# Prepare file version information
#
    cd $(ROOT)\src\developer\inst
    $(MKVER_U) download.in download.mak

#
# Build the installation archive
#

    $(MAKE) -fdownload.mak candle
    $(WIXLIGHT) -sice:ICE91 -sice:ICE60 -dWixUILicenseRtf=License.rtf -out keymandeveloper.msi -ext WixUIExtension $(DEVELOPER_FILES)
    $(MAKE) -fdownload.mak clean-heat

#
# Sign the installation archive
#

    $(SIGNCODE) /d "Keyman Developer" keymandeveloper.msi

#
# Copy the installation archive
#

    cd $(ROOT)\src\developer\inst
    $(MAKE) -fdownload.mak

pull-server:
#
# Requires us to build developer/server (which in turn build kmw) in order to
# get the output files If GIT_BASH_FOR_KEYMAN variable is not defined, then will
# just throw up a new window to do the build. This means we lose logging and
# potentially exit values as well. So GIT_BASH should be set for build agents,
# etc, to something like
#
# GIT_BASH_FOR_KEYMAN="C:\Program Files\Git\bin\bash.exe" --init-file
# "c:\Program Files\Git\etc\profile" -l
#
    cd $(ROOT)\..\developer\server

!ifdef GIT_BASH_FOR_KEYMAN
    $(GIT_BASH_FOR_KEYMAN) build.sh --production --test
!else
    start /wait ./build.sh --production --test
!endif

build:
    @rem

signcode:
    @rem

wrap-symbols:
    @rem

clean:
    cd $(ROOT)\src\developer\inst
    -del /Q download.mak
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
    $(MKVER_U) download.in download.mak

    $(MAKE) -fdownload.mak test-releaseexists

install:
    @rem

# ----------------------------------------------------------------------

!include ..\..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
