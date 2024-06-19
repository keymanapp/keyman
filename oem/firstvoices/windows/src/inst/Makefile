#
# FirstVoices installation builder Makefile
#

!include ..\Defines.mak

# ----------------------------------------------------------------------

DESKTOP_FILES=firstvoices.wixobj desktopui.wixobj
MSI=firstvoices.msi
EXE_ZIP=firstvoices.zip
EXE=firstvoices.exe
KMP=fv_all.kmp
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
    $(MKVER_U) download.in download.mak

    cd $(FVROOT)\src\xml
    $(MKVER_U) sentry.init.js.in sentry.init.js

#
# Build FirstVoices keyboards fv_all.kmp
#

    cd $(KEYMAN_ROOT)\..\keyboards

!ifdef GIT_BASH_FOR_KEYMAN
    $(GIT_BASH_FOR_KEYMAN) build.sh -k release/fv
    $(GIT_BASH_FOR_KEYMAN) build.sh -k release/packages/fv_all
!else
    start /wait ./build.sh -k release/fv
    start /wait ./build.sh -k release/packages/fv_all
!endif

    copy release\packages\fv_all\build\fv_all.kmp $(FVROOT)\src\inst
    cd $(FVROOT)\src\inst

desktop: prereq
    rem compile .msi
    $(MAKE) -fdownload.mak candle-desktop
    $(WIXLIGHT) -dWixUILicenseRtf=License.rtf -out $(MSI) -ext WixUIExtension $(DESKTOP_FILES)
    $(SIGNCODE) /d $(APPTITLE) $(MSI)

    rem build self-extracting archive
    $(MAKE) -fdownload.mak setup-inf
    $(WZZIP) $(EXE_ZIP) $(MSI) license.html setup.inf $(TITLEIMAGE) $(KMP)
    -del setup.inf
    $(COPY) /b $(ROOT)\bin\desktop\setup-redist.exe + $(EXE_ZIP) $(EXE)
    -del $(EXE_ZIP)

    rem sign and copy files
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
    -del /Q $(KMP)

check:
    if not exist $(ROOT)\src\engine\inst\keymanengine.msm $(MAKE) check-engine
    if not exist $(KEYMAN_ROOT)\common\windows\bin\data\unicodedata.mdb $(MAKE) check-unicodedata


check-engine:
    cd $(ROOT)\src\engine\inst
    $(MAKE)

check-unicodedata:
    cd $(KEYMAN_ROOT)\common\windows\data
    $(MAKE)

# ----------------------------------------------------------------------

!include $(ROOT)\src\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
