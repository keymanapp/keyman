#
# FirstVoices installation builder Makefile
#

!include ..\Defines.mak

# ----------------------------------------------------------------------

DESKTOP_FILES=firstvoices.wixobj desktopui.wixobj
MSI=firstvoices.msi
EXE=firstvoices.exe
APPTITLE="Keyman for FirstVoices"
TITLEIMAGE=setuptitle.png

# If doing manual build of the project, manual-setup is the default target
manual-setup: check setup

# During an automated build, `make setup` is called, so `check` target is never used
setup: desktop

prereq: 
    #
    # Prepare make file to copy versioned filenames to release folder
    #

    cd $(FVROOT)\src\inst
    $(PROGRAM)\buildtools\mkver -v -u download.in download.mak $(ROOT)\src\desktop\version.txt

desktop: prereq
    $(MAKE) -fdownload.mak candle-desktop
    $(WIXLIGHT) -dWixUILicenseRtf=License.rtf -out $(MSI) -ext WixUIExtension $(DESKTOP_FILES) 
    $(SIGNCODE) /d $(APPTITLE) $(MSI)
    $(ROOT)\bin\buildtools\buildpkg -m $(MSI) -s $(ROOT)\bin\desktop -l license.html -a $(APPTITLE) -i $(TITLEIMAGE)
    $(SIGNCODE) /d $(APPTITLE) $(EXE)
    $(MAKE) -fdownload.mak copyredist-desktop

clean:
    cd $(FVROOT)\src\inst
    if exist download.mak $(MAKE) -fdownload.mak clean
    -del /Q *.msi
    -del /Q *.msp
    -del /Q *.exe
    -del /Q *.wixobj
    -del /Q *.wixpdb
    -del /Q *.log
    -del /Q *.pcp
    -del /Q download.mak
    -del /Q desktopui.wxs

check:
    if not exist $(ROOT)\src\engine\inst\keymanengine.msm $(MAKE) check-engine
    if not exist $(ROOT)\src\global\inst\data\unicodedata.mdb $(MAKE) check-unicodedata


check-engine:
    cd $(ROOT)\src\engine\inst
    $(MAKE)

check-unicodedata:
    cd $(ROOT)\src\global\inst
    $(MAKE)

# ----------------------------------------------------------------------

!include $(ROOT)\src\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
	