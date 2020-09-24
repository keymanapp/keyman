#
# Keyman root Makefile
#

# ----------------------------------------------------------------------

default:
    @echo The following make targets are available:
    @echo .   build                 builds everything except help files (-DBUILDHELP to build help)
    @echo .   install               installs packages into Delphi IDE (run after a build)
    @echo .   clean                 removes all temp files
    @echo .   release               make a release
    @echo .   web-install           sends the documentation to the website
    @echo .   test-certificate      create a test certificate + CA (and install test CA)
    @echo .   signcode              sign the build artifacts
    @echo.
    @echo .   Build flags:
    @echo .         DEBUG           Include debug information
    @echo .         SC_TIMESTAMP    Don't timestamp certificates (for internal releases only, offline)
    @echo .         BUILDHELP       Build the helpfiles (implicit when 'make release')
    @echo .         LINT            Turns on $MESSAGE hints and warnings and all extra string warnings
    @echo .         QUIET           Ignore hints and warnings
    @echo .         RELEASE_OEM     Build executables for third party products in oem folder (e.g. FirstVoices)
    @echo.
    @echo .   To view help on the build process, in /src/inst, run:
    @echo .         make help
    @echo.

# ----------------------------------------------------------------------

!ifdef QUICK_BUILD_KEYMAN
TARGETS=ext global buildtools engine desktop
MANIFESTS=engine desktop
!else
TARGETS=ext global buildtools developer engine desktop support
MANIFESTS=engine desktop developer
!endif

RELEASE_TARGETS=desktop
CLEANS=test unit-tests cleanroot

EXCLUDEPATHDEFINES=1
BUILDPREREQ=global-versions

!include Header.mak

# ----------------------------------------------------------------------

cleanroot:
    -del tds.zip
    -del tds.exe
    -del PathDefines.mak
    -del version.txt

global: build-tools
    cd $(ROOT)\src\global
    $(MAKE) $(TARGET)
    cd $(ROOT)\src

ext: build-tools
    cd $(ROOT)\src\ext
    $(MAKE) $(TARGET)
    cd $(ROOT)\src

buildtools: global
    cd $(ROOT)\src\buildtools
    $(MAKE) $(TARGET)
    cd $(ROOT)\src

developer: global
    cd $(ROOT)\src\developer
    $(MAKE) $(TARGET)
    cd $(ROOT)\src

engine: global message-constants
    cd $(ROOT)\src\engine
    $(MAKE) $(TARGET)
    cd $(ROOT)\src

desktop: global message-constants
    cd $(ROOT)\src\desktop
    $(MAKE) $(TARGET)
    cd $(ROOT)\src

support: global
    cd $(ROOT)\src\support
    $(MAKE) $(TARGET)
    cd $(ROOT)\src

test:
    cd $(ROOT)\src\test
    $(MAKE) $(TARGET)
    cd $(ROOT)\src

unit-tests:
    cd $(ROOT)\src\unit-tests
    $(MAKE) $(TARGET)
    cd $(ROOT)\src

# ----------------------------------------------------------------------
# release
# ----------------------------------------------------------------------

release: clean-pathdefines test-release-cef-path test-releasebuildcheck test-uiaccess test-klog test-releaseexists test-cert test-hhc test-7zip test-wix clean
    cd $(ROOT)\src
    $(MAKE) global-versions
    $(MAKE) build-release-wrapper
    # $(MAKE) clean  <-- this makes it harder to do OEM builds
    cd $(ROOT)\src

clean-pathdefines: test-releasebuildcheck
    # test-releasebuildcheck builds devtools for us
    $(PROGRAM)\buildtools\devtools -ri

#
# Test that the CEF file path is accessible from env var KEYMAN_CEF4DELPHI_ROOT
#
test-release-cef-path:
    @if not exist $(KEYMAN_CEF4DELPHI_ROOT)\libcef.dll echo The environment variable KEYMAN_CEF4DELPHI_ROOT is invalid. File libcef.dll could not be found.
    @if not exist $(KEYMAN_CEF4DELPHI_ROOT)\libcef.dll exit 1

#
# Test that certificate checking is enabled and that certificate is accessible
#

test-cert:
    copy $(SC_PFX_SHA1) $(ROOT)\src\pfx.tmp
    del $(ROOT)\src\pfx.tmp

#
# Test that hhc is installed
#

test-hhc:
    if not exist $(HHC) exit 1

#
# Test that WiX is installed
#

test-wix:
    if not exist $(WIXPATH)\candle.exe exit 1

#
# Test that 7-zip is installed
#

test-7zip:
    if not exist $(WZZIPPATH) exit 1

#
# Test that klog is disabled
#

test-klog: mkver-app
    cd $(ROOT)\src\buildtools\test-klog
    $(MAKE) verify
    cd $(ROOT)\src

#
# Test that we haven't already built this release
#

test-releaseexists: mkver-app
    cd $(ROOT)\src\developer\inst
    $(MAKE) test-releaseexists
    cd $(ROOT)\src

#
# Test various release prerequisites, e.g. Debug reg key not set
#

test-releasebuildcheck: global-versions
    cd $(ROOT)\src\buildtools\devtools
    $(MAKE) test-releasebuildcheck
    cd $(ROOT)\src

#
# Test uiaccess property in keyman manifest.xml and keyman64 manifest.xml
#

test-uiaccess:
    cd $(ROOT)\src\engine\keyman
    $(MAKE) test-uiaccess
    cd $(ROOT)\src\engine\keymanx64
    $(MAKE) test-uiaccess
    cd $(ROOT)\src

#
# Build Keyman projects for coverity; call this with
#   make test-coverity
# * Set environment variable COVBUILD to path to cov-build.exe
# * tar must be on path
#

test-coverity:
    cd $(ROOT)\src
    if exist cov-int rd /s/q cov-int
    if exist keyman-windows-coverity.tgz del keyman-windows-coverity.tgz
    $(COVBUILD) --dir cov-int $(MAKE) test-coverity-internal
    tar cvzf keyman-windows-coverity.tgz cov-int
    rd /s/q cov-int
    echo Manual upload keyman-windows-coverity.tgz to coverity
    # TODO: automated upload as part of build

test-coverity-internal:
    cd $(ROOT)\src\developer\kmanalyze
    $(MAKE) build
    cd $(ROOT)\src\developer\kmcmpdll
    $(MAKE) build
    cd $(ROOT)\src\engine\keyman32
    $(MAKE) build
    cd $(ROOT)\src\engine\keyman64
    $(MAKE) build
    cd $(ROOT)\src\engine\keymanx64
    $(MAKE) build
    cd $(ROOT)\src\engine\kmtip
    $(MAKE) build
    cd $(ROOT)\src\engine\mcompile
    $(MAKE) build

# Build a release

build-release-wrapper: clean-output global-versions
    cd $(ROOT)\src
    $(MAKE) dirs
    $(MAKE) build-tools
!ifdef QUICK_BUILD_KEYMAN
    $(MAKE) build
    $(MAKE) build-release
    $(MAKE) signcode
!else
    $(MAKE) build unit-tests
    $(MAKE) build-release
    $(MAKE) signcode
    $(MAKE) -DTARGET=test test
    $(MAKE) backup
!endif

    cd $(ROOT)\src\engine\inst
    $(MAKE)

    cd $(ROOT)\src\desktop\inst
    $(MAKE) setup

!ifdef QUICK_BUILD_KEYMAN
!else
    cd $(ROOT)\src
    $(MAKE) build-release-wrapper-oem

    cd $(ROOT)\src\developer\inst
    $(MAKE)

    cd $(ROOT)\src\buildtools\inst
    $(MAKE)
!endif

build-release-wrapper-oem:
!ifdef RELEASE_OEM
    cd $(ROOT)\..\oem\firstvoices\windows\src\inst
    $(MAKE) setup
!else
    echo Not building OEM releases
!endif

#
# Tools needed to complete a build
#

global-versions: mkver-app
    # Build our root version.txt
    cd $(ROOT)\src

    $(MKVER_U) $(ROOT)\src\global\inc\keymanversion_build.in $(ROOT)\src\global\inc\keymanversion_build.h
    $(MKVER_U) $(ROOT)\src\global\delphi\general\keymanversion_build.in $(ROOT)\src\global\delphi\general\keymanversion_build.inc

build-tools: global-versions
    cd $(ROOT)\src\buildtools\devtools
    $(MAKE)

    # We'll build a default PathDefines.mak if it is missing here so we can
    # go on and do the rest of the build
    $(PROGRAM)\buildtools\devtools -ti

message-constants: build-tools
    $(PROGRAM)\buildtools\devtools -buildmessageconstants $(ROOT)\src\desktop\kmshell\xml\strings.xml $(ROOT)\src\global\delphi\cust\MessageIdentifierConsts.pas

#
# Remove all files from output folders to ensure no legacy files left in a release
#

clean-output:
    rd /s/q $(ROOT)\bin
    rd /s/q $(ROOT)\debug
    rd /s/q $(ROOT)\build
    rd /s/q $(ROOT)\lib

# ----------------------------------------------------------------------
# install
# ----------------------------------------------------------------------

web-install:
    cd $(ROOT)\src\desktop\help
    $(MAKE) html install

    cd $(ROOT)\src\developer\help
    $(MAKE) html install

# ----------------------------------------------------------------------

test-certificate:
    cd $(ROOT)\src\buildtools\certificates
    -del KeymanTestCA-sha1.*
    -del KeymanTest-sha1.*
    -del KeymanTest-sha256.*
    -del KeymanTestCA-sha256.*

    makecert -r -pe -n "CN=Keyman Test CA SHA1" -ss CA -sr CurrentUser -a sha1 -cy authority -sky signature -sv KeymanTestCA-sha1.pvk KeymanTestCA-sha1.cer
    certutil -user -addstore Root KeymanTestCA-sha1.cer
    makecert -pe -n "CN=Keyman Test Certificate SHA1" -a sha1 -cy end -sky signature -ic KeymanTestCA-sha1.cer -iv KeymanTestCA-sha1.pvk -sv KeymanTest-sha1.pvk KeymanTest-sha1.cer
    pvk2pfx -pvk KeymanTest-sha1.pvk -spc KeymanTest-sha1.cer -pfx KeymanTest-sha1.pfx

    makecert -r -pe -n "CN=Keyman Test CA" -ss CA -sr CurrentUser -a sha256 -cy authority -sky signature -sv KeymanTestCA-sha256.pvk KeymanTestCA-sha256.cer
    certutil -user -addstore Root KeymanTestCA-sha256.cer
    makecert -pe -n "CN=Keyman Test Certificate" -a sha256 -cy end -sky signature -ic KeymanTestCA-sha256.cer -iv KeymanTestCA-sha256.pvk -sv KeymanTest-sha256.pvk KeymanTest-sha256.cer
    pvk2pfx -pvk KeymanTest-sha256.pvk -spc KeymanTest-sha256.cer -pfx KeymanTest-sha256.pfx

!include Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
